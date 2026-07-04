/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.commands.suggestion.FactionSuggestions;
import de.markusbordihn.easynpc.data.faction.FactionDataEntry;
import de.markusbordihn.easynpc.data.saveddata.FactionData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.handler.FactionHandler;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.ColorArgument;

public class FactionCommand extends Command {

  private static final String FACTION_ARG = "faction";
  private static final String HOSTILE_FACTION_ARG = "hostile_faction";

  private FactionCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("faction")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(
            Commands.literal("create")
                .then(
                    Commands.argument(NAME_ARG, StringArgumentType.word())
                        .executes(
                            context ->
                                createFaction(
                                    context.getSource(),
                                    StringArgumentType.getString(context, NAME_ARG)))))
        .then(
            Commands.literal("delete")
                .then(
                    Commands.argument(NAME_ARG, StringArgumentType.word())
                        .suggests(FactionSuggestions::suggest)
                        .executes(
                            context ->
                                deleteFaction(
                                    context.getSource(),
                                    StringArgumentType.getString(context, NAME_ARG)))))
        .then(Commands.literal("list").executes(context -> listFactions(context.getSource())))
        .then(
            Commands.literal("color")
                .then(
                    Commands.argument(NAME_ARG, StringArgumentType.word())
                        .suggests(FactionSuggestions::suggest)
                        .then(
                            Commands.argument(COLOR_ARG, ColorArgument.color())
                                .executes(
                                    context ->
                                        setFactionColor(
                                            context.getSource(),
                                            StringArgumentType.getString(context, NAME_ARG),
                                            ColorArgument.getColor(context, COLOR_ARG))))))
        .then(
            Commands.literal("hostile")
                .then(
                    Commands.literal("add")
                        .then(
                            Commands.argument(FACTION_ARG, StringArgumentType.word())
                                .suggests(FactionSuggestions::suggest)
                                .then(
                                    Commands.argument(
                                            HOSTILE_FACTION_ARG, StringArgumentType.word())
                                        .suggests(FactionSuggestions::suggest)
                                        .executes(
                                            context ->
                                                addHostileFaction(
                                                    context.getSource(),
                                                    StringArgumentType.getString(
                                                        context, FACTION_ARG),
                                                    StringArgumentType.getString(
                                                        context, HOSTILE_FACTION_ARG))))))
                .then(
                    Commands.literal("remove")
                        .then(
                            Commands.argument(FACTION_ARG, StringArgumentType.word())
                                .suggests(FactionSuggestions::suggest)
                                .then(
                                    Commands.argument(
                                            HOSTILE_FACTION_ARG, StringArgumentType.word())
                                        .suggests(FactionSuggestions::suggest)
                                        .executes(
                                            context ->
                                                removeHostileFaction(
                                                    context.getSource(),
                                                    StringArgumentType.getString(
                                                        context, FACTION_ARG),
                                                    StringArgumentType.getString(
                                                        context, HOSTILE_FACTION_ARG))))))
                .then(
                    Commands.literal("list")
                        .then(
                            Commands.argument(FACTION_ARG, StringArgumentType.word())
                                .suggests(FactionSuggestions::suggest)
                                .executes(
                                    context ->
                                        listHostileFactions(
                                            context.getSource(),
                                            StringArgumentType.getString(context, FACTION_ARG))))))
        .then(
            Commands.literal("set")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.argument(NAME_ARG, StringArgumentType.word())
                                .suggests(FactionSuggestions::suggest)
                                .executes(
                                    context ->
                                        setNPCFaction(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            StringArgumentType.getString(context, NAME_ARG))))))
        .then(
            Commands.literal("get")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                getNPCFaction(
                                    context.getSource(),
                                    EasyNPCArgument.getEntity(context, NPC_TARGET_ARG)))))
        .then(
            Commands.literal("clear")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                clearNPCFaction(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(
                                        context, NPC_TARGET_ARG)))));
  }

  private static int createFaction(CommandSourceStack context, String factionName) {
    if (!FactionData.get().createFaction(factionName)) {
      return sendFailureMessage(context, "Faction '" + factionName + "' already exists!");
    }
    return sendSuccessMessage(context, "Created faction '" + factionName + "'.");
  }

  private static int deleteFaction(CommandSourceStack context, String factionName) {
    if (!FactionData.get().removeFaction(factionName)) {
      return sendFailureMessage(context, "Faction '" + factionName + "' does not exist!");
    }
    return sendSuccessMessage(context, "Deleted faction '" + factionName + "'.");
  }

  private static int listFactions(CommandSourceStack context) {
    return sendSuccessMessage(
        context, "Factions: " + String.join(", ", FactionData.get().getFactionNames()));
  }

  private static int setFactionColor(
      CommandSourceStack context, String factionName, ChatFormatting color) {
    FactionDataEntry factionDataEntry = FactionData.get().getFaction(factionName);
    if (factionDataEntry == null) {
      return sendFailureMessage(context, "Faction '" + factionName + "' does not exist!");
    }
    factionDataEntry.setColor(color);
    FactionData.get().setDirty();
    return sendSuccessMessage(
        context, "Set color of faction '" + factionName + "' to " + color.getName() + ".");
  }

  private static int addHostileFaction(
      CommandSourceStack context, String factionName, String hostileFactionName) {
    if (!FactionData.get().addHostileFaction(factionName, hostileFactionName)) {
      return sendFailureMessage(
          context,
          "Unable to add hostile faction '"
              + hostileFactionName
              + "' to faction '"
              + factionName
              + "'!");
    }
    return sendSuccessMessage(
        context, "Faction '" + factionName + "' is now hostile to '" + hostileFactionName + "'.");
  }

  private static int removeHostileFaction(
      CommandSourceStack context, String factionName, String hostileFactionName) {
    if (!FactionData.get().removeHostileFaction(factionName, hostileFactionName)) {
      return sendFailureMessage(
          context,
          "Unable to remove hostile faction '"
              + hostileFactionName
              + "' from faction '"
              + factionName
              + "'!");
    }
    return sendSuccessMessage(
        context,
        "Faction '" + factionName + "' is no longer hostile to '" + hostileFactionName + "'.");
  }

  private static int listHostileFactions(CommandSourceStack context, String factionName) {
    FactionDataEntry factionDataEntry = FactionData.get().getFaction(factionName);
    if (factionDataEntry == null) {
      return sendFailureMessage(context, "Faction '" + factionName + "' does not exist!");
    }
    return sendSuccessMessage(
        context,
        "Faction '"
            + factionName
            + "' is hostile to: "
            + String.join(", ", factionDataEntry.getHostileFactions()));
  }

  private static int setNPCFaction(
      CommandSourceStack context, EasyNPC<?> easyNPC, String factionName) {
    if (easyNPC == null) {
      return 0;
    }
    if (!FactionHandler.setFaction(easyNPC, factionName)) {
      return sendFailureMessage(context, "Failed to set faction for " + easyNPC);
    }
    return sendSuccessMessage(
        context, "Faction of " + easyNPC + " was changed to '" + factionName + "'.");
  }

  private static int getNPCFaction(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return 0;
    }
    FactionDataCapable<?> factionData = easyNPC.getEasyNPCFactionData();
    if (factionData == null || !factionData.hasFactionName()) {
      return sendFailureMessage(context, "No faction assigned to " + easyNPC);
    }
    return sendSuccessMessage(
        context, easyNPC + " is member of faction '" + factionData.getFactionName() + "'.");
  }

  private static int clearNPCFaction(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return 0;
    }
    if (!FactionHandler.removeFaction(easyNPC)) {
      return sendFailureMessage(context, "Failed to remove faction from " + easyNPC);
    }
    return sendSuccessMessage(context, "Faction of " + easyNPC + " was removed.");
  }
}
