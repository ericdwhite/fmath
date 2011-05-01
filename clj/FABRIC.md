
Setup
-----
Download and install Fabric 1.0.1

sudo pip install fabric

Deploy
------
1. Create a file .fabricrc with:

# Provide a user to run under and an install location.
site_root = /home/auser/fmath


2. Create initial directories on the remote server

./bin/fabric -H example.com:22 -u auser setup 

3. Deploy the latest version from the 'live' github branch

./bin/fabric -H example.com:22 -u auser deploy

4. Symlink the latest release to current

./bin/fabric -H example.com:22 -u auser symlink 

5. Restart the Clojure application (the User must have sudo access)

./bin/fabric -H example.com:22 -u auser restart

(assumes etc/mwc.conf as been installed in /etc/init and Upstart knows about it!)
