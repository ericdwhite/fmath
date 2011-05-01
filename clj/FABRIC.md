
Setup
-----
Download and install Fabric 1.0.1

sudo pip install fabric

Deploy
------
1. Create a file .fabricrc with:

# Provide a user to run under and an install location.
user = auser
site_root = /home/auser/fmath


2. Create initial directories on the remote server

./bin/fabric setup -H example.com:22

3. Deploy the latest version from the 'live' github branch

./bin/fabric deploy -H example.com:22

4. Symlink the latest release to current

./bin/fabric symlink -H example.com:22

5. Restart the Clojure application

restart mwc

(assumes etc/mwc.conf as been installed in /etc/init and Upstart knows about it!)
