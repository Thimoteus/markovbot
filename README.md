#markovbot

## Dependencies
`purescript`, `node`, `npm`, `pulp`

## Using

Create a `data/` directory and move an IRC log (I'm using Weechat. I doubt another client's logs will work out-of-the-box, nor do I guarantee any Weechat user's logs will, either.) to `data/in.txt`. Choose a username (maybe your own) and put it into `data/name`, i.e. `echo "myusername" > data/name`.

Run `pulp main --Distiller`, this will parse the IRC log and place only messages in the log that were sent by the name in `data/name` into `data/$name/out`. At this point you should split that file into smaller files prefixed with the name you chose. I found sublogs of about 400 lines or more would result in a stack overflow, so I prefer sublogs of 125 lines.

Now edit `src/Main.purs` with relevant info, then `pulp run` and you're good to go.
