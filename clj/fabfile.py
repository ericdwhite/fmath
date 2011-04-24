import os
import sys
import traceback
from contextlib import contextmanager
from fabric.api import run, local
from fabric.operations import put
from fabric.context_managers import lcd, cd
from fabric.utils import abort
from fabric.state import output, env

#
# The following is based on default capistrano layouts
# and heavily inspired by Lee Hambley's railsless-deploy
#   https://github.com/leehambley/railsless-deploy
#
#  There are some assumptions about how this works:
#    1. It uses a local maven repository to source artifacts.  These
#       artifacts are then tarball'ed and sent to the server.
#    2. The repository is a git repository reachable from
#       the deployment server.
#
#  Key methods:
#    info - describes the necessary environment settings in _fabricrc
#    setup - must be run before the first deployment
#    create_revision - Creates a tar.gz, this should be customized.
#    deploy - deploys a tarball created with create_revision.
def info():
    """
    Displays the key attributes to be setup in fabric RCFILE (See: fab -c).
    """
    print

    print "site_root = /app/webapp_name"
    print """
          All other directories will be created under this root.  This allows
          for multiple applications to be deployed on the same host.

          All the following paths will be relative to this path unless they
          start with a '/'.
          """

    print "releases_path = releases"
    print """
          Releases Path: a directory which will contain each deployed
          build.  The Current Path will point to a sub-folder of this path.

          $site_root/releases/20110623_162311
          $site_root/releases/20110622_100451

          $site_root/current -> $site_root/releases/20110623_162311
          """

    print "current_path = current"
    print """
          Current Path: a symlink point to a release in Releases Path.  Changing
          this symlink is the key method of rollback.
    
          $site_root/current -> $site_root/releases/20110623_162343
          """

    print "shared_path = shared"
    print """
          Shared Path: a directory that contains common items such as logs,
          temporary space, etc.
          """

    print "logs_path = logs"
    print """
          Logs Path: a directory to contain logs.
          """

    print "public_path = public"
    print """
          Public Path: a directory that will contain static content that can
          be exposed to a webserver directly.
          """

    print "tmp_path = tmp"
    print """
          Temporary Path: a directory that will be used for deployment 
          uploads.
          """


    print "repo = git@github.com:ericdwhite/fmath.git"
    print """
          Repository: A repository accessible from the deployment machine.
          """

    print "repo_branch = live"
    print """
          Repository Branch: The branch to clone from the repository. 
          """

@contextmanager
def _rollback_handler(rollback):
    """
    This calls 'func' and if an exception
    is raised in the processing of 'func'
    then it calls the 'rollback' function to
    unwind changes.  The 'rollback' will
    also be wrapped in try/except.  It's best
    to do the minimal work possible in the
    'rollback' function as the underlying
    reason for 'func' failing may also
    impact the 'rollback' function, for 
    example not enough disk space. """
    try:
        yield
    except:
        if output.aborts:
            try:
                print >> sys.stderr, "--> Exception %s" % sys.exc_info()[0]
                print >> sys.stderr, "-->           %s" % traceback.print_exc(15)
                print >> sys.stderr, "--> Attempting to rollback."
            finally:
                try:
                    rollback()
                    print >> sys.stderr, "    --> Rollback Handler SUCCEEDED"
                except:
                    print >> sys.stderr, "    --> Rollback Handler FAILED **"
                    print >> sys.stderr, sys.exc_info()
                finally:
                    abort("Run Failed.")

def _status(msg):
    if output.status:
        print msg

def _get_attr(attr):
    try:
        return env[attr]
    except NameError:
        abort("RCFILE/env does not define: %s" % attr)
    except KeyError:
        abort("RCFILE/env does not define: %s" % attr)

def _archive_info(archive):
     name = os.path.basename(archive)
     app, stamp = name.split('-')  
     stamp = stamp.split('.')[0]
     return (name, app, stamp) 
 
def _init():
    if not env.has_key('inited'):
        _status("Initializing.")
        _merge_paths()
        env.inited = True

def _merge_paths():
    site_root = _get_attr('site_root')
    env.releases_path = os.path.join(site_root, _get_attr('releases_path'))
    env.current_path = os.path.join(site_root, _get_attr('current_path'))
    env.shared_path = os.path.join(site_root, _get_attr('shared_path'))
    env.logs_path = os.path.join(site_root, _get_attr('logs_path'))
    env.public_path = os.path.join(site_root, _get_attr('public_path'))
    env.tmp_path = os.path.join(site_root, _get_attr('tmp_path'))
    
def setup():
    """
    This creates the initial directory structure as defined in the RCFILE.  This
    is expected to be called once, when a new site is being commissioned.
    """
    _status("Creating directories.")
    _init()

    run("mkdir -p %(site_root)s" % env)
    run("mkdir -p `dirname %(current_path)s`" % env)
    run("mkdir -p %(shared_path)s" % env)
    run("mkdir -p %(logs_path)s" % env)
    run("mkdir -p %(releases_path)s" % env)
    run("mkdir -p %(public_path)s" % env)
    run("mkdir -p %(tmp_path)s" % env)
    run("tree %(site_root)s" % env)

def create_revision():
    """
    Creates a tar.gz of the project to be transfered to
    the remote host.  This method creates a temporary
    directory to work with a clean clone of the repository.
    This avoids by accident deploying something not checked in.
    """
    _init();

    if env.has_key('archive'):
        archive = _get_attr('archive')
        _status("Archive: %s"  % archive)
        return archive

    #
    # The following is specific to this project
    # and would need to be changed if this file is
    # used for anything else.
    import tempfile
    import datetime
    tempdir = tempfile.mktemp();
    archive_date = datetime.datetime.utcnow().isoformat('_').replace('-', '').replace(':', '').split('.')[0]
    archive = os.path.join(tempdir, "fmath-%s.tar.gz" % archive_date)
    local("mkdir -p %s" % tempdir)
    with lcd(tempdir):
        repo = _get_attr('repo')
        branch = _get_attr('repo_branch')
        local("git clone %s repo" % repo)
        with lcd('repo/clj'):
            local("git checkout %s" % branch)
            local('lein deps')
            local('LEIN_SNAPSHOTS_IN_RELEASE=true lein jar')
            local('git rev-list --max-count=1 HEAD > REVISION')
            local("tar -czf %s `cat FILES`" % archive)
            _status("Archive: %s"  % archive)
            env.archive = archive
            return archive

def _deploy_rb():
    _status("Unlinking current.")
    
def deploy():
    """
    Deploys the latest version of the code making use of:
      create_revision
      symlink

    This assumes there is no top level folder in the application tarball, and
    the tarball conforms to APP-DATE_TIME.EXT.
    """
    _init()
    create_revision()

    with _rollback_handler(_deploy_rb):
        _status("Starting deployment.")
        archive = _get_attr('archive')
        tmp = _get_attr('tmp_path')
        put(archive, tmp)
        name, app, stamp = _archive_info(archive)
        _status("Archive info:: App: %s Timestamp: %s File: %s" % (app, stamp, name))
        release_path = os.path.join(_get_attr('releases_path'), stamp)
        run("mkdir -p %s" % release_path)
        with cd(release_path):
            run("tar -xzf %s" % os.path.join(tmp, name))
        run("tree -d %s" % release_path)

def symlink():
    """
    Creates a symlink to the latest release found using 'ls -xr'
    """
    _init()

    release_path = _get_attr('releases_path')
    with cd(release_path):
        output = run("ls -xr")
        releases = output.split()
        releases.reverse()
        release = releases.pop()
        release = os.path.join(release_path, release)
        _status("Symlinking to latest release: %s" % release)

    with cd(_get_attr('site_root')):
        run("ln -sf %s %s" %(release, _get_attr('current_path')))

