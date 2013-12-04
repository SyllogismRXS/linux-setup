Edit the crontab
=====================

$ crontab -e

Add the following lines:

0,15,30,45 * * * * /home/syllogismrxs/git-repos/linux-setup/scripts/google2org > /home/syllogismrxs/logs/google2org.log 2>&1         
1,16,31,46 * * * * /home/syllogismrxs/git-repos/linux-setup/scripts/org2google > /home/syllogismrxs/logs/org2google.log 2>&1 

