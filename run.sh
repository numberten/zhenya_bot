#!/bin/bash
# Handles automatically restarting and updating zhenya_bot

# Make sure that we are in the zhenya_bot source directory. It should be the
# location of the script.
DIR=`dirname $0`
cd $DIR

# Run the bot and save the exit status. The exit status determines if the bot is
# restarted, updated or simply terminated.
ghc -o bot Main.hs && ./bot $@
status=`echo -n $?`
if [ "$status" == 0 ]; then
    echo "Stopping..."
    exit 0
elif [ "$status" == 100 ]; then
    echo "Restarting..."
    $DIR/run.sh $@
elif [ "$status" == 101 ]; then
    echo "Updating..."
    git pull
    $DIR/run.sh $@
# There were likely syntax errors. In this case, simply exit.
else
    exit $status
fi
