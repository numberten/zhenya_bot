zhenya_bot
==========

Irc bot, meant for use on a private irc server. Mostly humorous, with functionality slowly being added.
Conceived via [this tutorial](http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot) and breathed life into by the tarnished souls of many a bug who could not be dried.

commands
--------

   - !id <command>
      - zhenya_bot will privmsg <command> to channel
   - !uptime
      - privmsgs time spent in channel
   - !quit
      - terminates connection to irc server
   - !seen <person>
      - privmsgs time since <person> was last active
   - !ascend
      - attempts to +o speaker, with echo
   - !ding
      - attempts to +o speaker, without echo
   - !roll <number> 
      - privmsgs a random number between 1 and <number>
