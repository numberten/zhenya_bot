zhenya\_bot
===========

Zhenya_bot is an irc bot written in Haskell. While originally meant for use on a
private server, as of v1.0 zhenya_bot should be ready for general purpose use on
compatibility tested ircds.

Compatibility
-------------

As of v1.0 zhenya_bot is known to be compatible with the follow IRC daemons:
 - [IRCD-Hybrid](http://sourceforge.net/projects/ircd-hybrid/files/ircd-hybrid/) v7.2.2
 - [ircd-seven](https://dev.freenode.net/redmine/projects/ircd-seven) v1.1.3
 - [ggircd](https://github.com/fimad/ggircd) v1.0

While zhenya_bot may be compatible with other ircds, those listed above have
been shown to work as intended with all functionality as of v1.0.

Running
-------

The run.sh command handles launching, restarting, and updating the bot. The
following command line flags are accepted:

  - -s --server=HostName        The IRC server to connect to
  - -p --port=PortNumber        The port on which the IRC server is running
  - -n --nick=Nick              The nick that the bot should take
  - -c --channel=Channel        The channel that the bot should join
  - -? --help                   Display help message
  - -V --version                Print version information

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
   - !grep \[-c int\] \[-n nick\] \[-m matches\] \<regex\>
      - c flag indicates the number of lines above and below the match to print
      - n flag filters lines by a given nick
      - m flag number of matches to return
   - !list show \[list\]
      - privmsgs the list of lists
      - if passed a list name, privmsgs the contents of that list
   - !list add \[at index\] list \[element\]
      - adds a list to the list of lists
      - if passed an element, adds that element to the given list, possibly at a specific index
   - !list rm list \[element\]
      - removes the given list
      - if passed an element or index, removes that element/element at that index from the given list
   - !list check list element
      - adds or removes a strike through of the element in the given list
   - !list flush list
      - removes all striked through elements in the given list
   - !queens \<number\>
      - solves the [n-queens problem](http://en.wikipedia.org/wiki/Eight_queens_puzzle)
      - pretty prints a solution for n <= 12
    
