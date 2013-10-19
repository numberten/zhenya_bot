zhenya\_bot
==========

Irc bot, meant for use on a private irc server. Mostly humorous, with
functionality slowly being added. Conceived via [this
tutorial](http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot) and breathed
life into by the tarnished souls of many a bug who could not be dried. 

Running
-------

The run.sh command handles launching, restarting, and updating the bot. The
following command line flags are accepted:

  * -s --server=HostName        The IRC server to connect to
  * -p --port=PortNumber        The port on which the IRC server is running
  * -n --nick=Nick              The nick that the bot should take
  * -c --channel=Channel        The channel that the bot should join
  * -? --help                   Display help message
  * -V --version                Print version information

Note that the channel flag can be supplied multiple times to cause the bot to
join multiple channles.

Commands
---------

   - !id \<command\>
      - zhenya_bot will privmsg <command> to channel
   - !uptime
      - privmsgs time spent in channel
   - !quit
      - terminates connection to irc server
   - !restart
      - restarts the zhenya_bot
   - !update
      - restarts the zhenya_bot and performs a `git pull` before reconnecting
   - !seen \<nick\>
      - privmsgs time since \<nick\> last spoke
   - !ascend
      - attempts to +o speaker, with echo
   - !ding
      - attempts to +o speaker, without echo
   - !roll \<number\> 
      - privmsgs a random number between 1 and \<number\>
   - !be \<nick\>
      - zhenya_bot will attempt to say a sentence in the manner of \<nick\>, using a bigram model trained off irc logs
   - !files \<string\>
      - searches for \<string\> in a given file catalogue, reporting the top 5 results
   - !alias \[\<nick\>\]
      - reports all nick clusters seen by the zhenya_bot
      - only reports nick cluster containing \<nick\>, if optional \<nick\> argument is given
   - !define \<string\>
      - responds with "definition" of \<string\>
