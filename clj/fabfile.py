import sys
import os
from fabric.api import run
from fabric.utils import abort
from fabric.state import output, env

def _with_rollback(func, rollback):
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
        func()
    except:
        if output.aborts:
            try:
                print >> sys.stderr, "--> Exception %s" % sys.exc_info()[0]
                print >> sys.stderr, "--> Attempting to rollback."
            finally:
                try:
                    rollback()
                    print >> sys.stderr, "    --> Rollback SUCCEEDED."
                except:
                    print >> sys.stderr, "    --> Rollback FAILED."
                finally:
                    abort("Run Failed.")

def _status(msg):
    if output.status:
        print msg

#
# The following is based on default capistrano layouts
# and heavely inspired by Lee Hambley's railsless-deploy
#   https://github.com/leehambley/railsless-deploy
#

def info():
    """
    Displays the key attributes to be setup in fabric RCFILE (See: fab -c).
    """
    print

    print "site_root = /home/wsite/webapp_name"
    print """
          All other directories will be created under this root.  This allows
          for multiple applications to be deployed on the same host.

          All the following paths will be relative to this path unless they
          start with a '/'.
          """

    print "releases_path = releases"
    print """
          Releases Path: a directory which will contain each deployed
          build.  The Current Path will point to a subfolder of this path.

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

def _init():
    if not env.has_key('inited'):
        _status("Initializing.")
        _merge_paths()
        env.inited = True

def _get_attr(attr):
    try:
        return env[attr]
    except NameError:
        abort("RCFILE/env does not define: " % attr)

def _merge_paths():
    site_root = _get_attr('site_root')
    env.releases_path = os.path.join(site_root, _get_attr('releases_path'))
    env.current_path = os.path.join(site_root, _get_attr('current_path'))
    env.shared_path = os.path.join(site_root, _get_attr('shared_path'))
    env.logs_path = os.path.join(site_root, _get_attr('logs_path'))
    env.public_path = os.path.join(site_root, _get_attr('public_path'))
    
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
    run("tree %(site_root)s" % env)

