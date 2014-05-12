zhenya\_bot
===========

Zhenya_bot is an irc bot written in Haskell. While originally meant for use on a
private server, as of v1.0 zhenya_bot should be ready for general purpose use with
compatibility tested ircds.

Compatibility
-------------

As of v1.0 zhenya_bot is known to be compatible with the following IRC daemons:
 - [IRCD-Hybrid](http://sourceforge.net/projects/ircd-hybrid/files/ircd-hybrid/) v7.2.2
 - [ircd-seven](https://dev.freenode.net/redmine/projects/ircd-seven) v1.1.3
 - [ggircd](https://github.com/fimad/ggircd) v1.0

While zhenya_bot may be compatible with other ircds, only those listed above have
been shown to work as intended with all functionality as of v1.0.

Installation
------------

You'll need ghc (the Glasgow Haskell Compiler) and cabal (the Haskell package manager).

You can install them by following the links below:
  - [ghc](https://www.haskell.org/ghc/docs/6.10.1/html/users_guide/installing-bin-distrib.html)
  - [cabal](http://www.haskell.org/haskellwiki/Cabal-Install#Installation)

Or by installing the [haskell-platform](http://www.haskell.org/platform/) which includes both.

To run the bot you'll need to `git clone https://github.com/numberten/zhenya_bot.git` and execute
the run script mentioned below. The first time you run zhenya_bot, it may take cabal several
minutes to download and compile dependencies.

Running
-------

The `./run.sh` script handles launching, restarting, and updating the bot. The
following command line flags are accepted:

  - -s --server=HostName        The IRC server to connect to
  - -p --port=PortNumber        The port on which the IRC server is running
  - -n --nick=Nick              The nick that the bot should take
  - -c --channel=Channel        The channel that the bot should join
  - -? --help                   Display help message
  - -V --version                Print version information

Note that the channel flag can be supplied multiple times to cause the bot to
join multiple channels.

Commands
---------

   - !id \<command\>
      - zhenya_bot will privmsg \<command\> to channel
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
      - zhenya_bot will attempt to say a sentence in the manner of \<nick\>, using a bigram model trained off logs
   - !files \<string\>
      - searches for \<string\> in a given file catalogue, reporting the top 5 results
   - !alias \[\<nick\>\]
      - reports all nick clusters seen by the zhenya_bot
      - if optional \<nick\> argument is given, reports nick cluster containing \<nick\>
   - !define \<string\>
      - responds with the [urbandictionary](http://www.urbandictionary.com/) "definition" of \<string\>
   - !grep \[-c int\] \[-n nick\] \[-m matches\] \<regex\>
      - searches for matches in channel history that match \<regex\>
      - '-c' flag indicates the number of lines above and below the match to print
      - '-n' flag filters lines by a given nick
      - '-m' flag indicates the number of matches to return
   - !list show \[list\]
      - privmsgs the list of lists
      - if passed a list name, privmsgs the contents of that list
   - !list add \[at index\] list \[element\]
      - adds a list to the list of lists
      - if passed an element, adds that element to the given list
      - elements added to a specific list can be optionally added to a given index
   - !list rm list \[element|index\]
      - removes the given list
      - if passed an element or index, removes that element (or element at that index) from the given list
   - !list check list (element|index)
      - adds or removes a strike through of the element in the given list
      - alternatively excepts an element's index
   - !list flush list
      - removes all striked through elements in the given list
   - !queens \<number\>
      - solves the [n-queens problem](http://en.wikipedia.org/wiki/Eight_queens_puzzle)
      - pretty prints a solution for n <= 12

