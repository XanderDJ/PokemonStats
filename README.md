# PokemonStats

This project currently takes one or two textfiles through the command line containing mons on each line of the txt file. 
It will look these pokemons up and then calculate different speed stats based on their base speed stat. It then aggregates all pokemons into an excel file.
When given two text files it will compare both teams next to each other.

# Changes

This project needs to be changed into a discord bot that allows the use of dt commands like in showdown. But also allows you to get the aforementioned excel file through a text command in discord.
for example:

(prefix)prepare
team1:
- mienshao
- toxtricity

team2:
- kommo-o
- keldeo

This should upload an excel file to discord called team1vsteam2.xlsx that contains the aforementioned data.

(prefix)dt (pokemon|item|ability|move|nature)

should return a description of the requested data. This should be done in an embed message.

For utility purposes it should still be possible to call this project with commandline purposes to skip the discord bot part.

