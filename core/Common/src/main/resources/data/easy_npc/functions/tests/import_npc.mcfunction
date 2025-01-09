# Easy NPC Test Script
# by Markus Bordihn
#
tellraw @p {"text":"Import Professor Quiz NPC over datapack!", "color":"gold", "bold":true}
execute run easy_npc preset import data easy_npc:preset/humanoid/professor_quiz_datapack.npc.nbt ~ ~ ~
