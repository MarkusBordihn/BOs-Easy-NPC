/*
 * Copyright 2023 Markus Bordihn
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
import de.markusbordihn.easynpc.commands.suggestion.ColorSuggestions;
import de.markusbordihn.easynpc.commands.suggestion.NameVisibilitySuggestions;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.NameHandler;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;

public class NameCommand extends Command {

  private NameCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("name")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(
            Commands.literal("set")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.argument(NAME_ARG, StringArgumentType.string())
                                .executes(
                                    context ->
                                        setName(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            StringArgumentType.getString(context, NAME_ARG)))
                                .then(
                                    Commands.argument(COLOR_ARG, StringArgumentType.word())
                                        .suggests(ColorSuggestions.INSTANCE)
                                        .executes(
                                            context ->
                                                setNameWithColor(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    StringArgumentType.getString(context, NAME_ARG),
                                                    StringArgumentType.getString(
                                                        context, COLOR_ARG)))
                                        .then(
                                            Commands.argument(
                                                    VISIBILITY_ARG, StringArgumentType.word())
                                                .suggests(NameVisibilitySuggestions.INSTANCE)
                                                .executes(
                                                    context ->
                                                        setNameWithColorAndVisibility(
                                                            context.getSource(),
                                                            EasyNPCArgument.getEntityWithAccess(
                                                                context, NPC_TARGET_ARG),
                                                            StringArgumentType.getString(
                                                                context, NAME_ARG),
                                                            StringArgumentType.getString(
                                                                context, COLOR_ARG),
                                                            StringArgumentType.getString(
                                                                context, VISIBILITY_ARG))))))))
        .then(
            Commands.literal("color")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.argument(COLOR_ARG, StringArgumentType.word())
                                .suggests(ColorSuggestions.INSTANCE)
                                .executes(
                                    context ->
                                        setNameColor(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            StringArgumentType.getString(context, COLOR_ARG))))))
        .then(
            Commands.literal("visibility")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.argument(VISIBILITY_ARG, StringArgumentType.word())
                                .suggests(NameVisibilitySuggestions.INSTANCE)
                                .executes(
                                    context ->
                                        setNameVisibility(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            StringArgumentType.getString(
                                                context, VISIBILITY_ARG))))))
        .then(
            Commands.literal("clear")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                clearName(
                                    context.getSource(),
                                    EasyNPCArgument.getEntity(context, NPC_TARGET_ARG)))));
  }

  private static int setName(CommandSourceStack context, EasyNPC<?> easyNPC, String name) {
    if (easyNPC == null || name == null) {
      return 0;
    }

    if (NameHandler.setCustomName(easyNPC, name, -1, NameVisibilityType.ALWAYS)) {
      return sendSuccessMessage(context, "Set name of " + easyNPC + " to " + name);
    }
    return 0;
  }

  private static int setNameWithColor(
      CommandSourceStack context, EasyNPC<?> easyNPC, String name, String colorName) {
    if (easyNPC == null || name == null || colorName == null) {
      return 0;
    }

    ChatFormatting chatFormatting = parseChatFormatting(colorName);
    if (chatFormatting == null) {
      return sendFailureMessage(context, "Invalid color: " + colorName);
    }

    int color = chatFormatting.getColor() != null ? chatFormatting.getColor() : -1;
    if (NameHandler.setCustomName(easyNPC, name, color, NameVisibilityType.ALWAYS)) {
      return sendSuccessMessage(
          context,
          "Set name of "
              + easyNPC
              + " to "
              + name
              + " with color "
              + chatFormatting.name().toLowerCase());
    }
    return 0;
  }

  private static int setNameWithColorAndVisibility(
      CommandSourceStack context,
      EasyNPC<?> easyNPC,
      String name,
      String colorName,
      String visibilityName) {
    if (easyNPC == null || name == null || colorName == null || visibilityName == null) {
      return 0;
    }

    ChatFormatting chatFormatting = parseChatFormatting(colorName);
    if (chatFormatting == null) {
      return sendFailureMessage(context, "Invalid color: " + colorName);
    }

    NameVisibilityType visibility = parseNameVisibilityType(visibilityName);
    if (visibility == null) {
      return sendFailureMessage(context, "Invalid visibility: " + visibilityName);
    }

    int color = chatFormatting.getColor() != null ? chatFormatting.getColor() : -1;
    if (NameHandler.setCustomName(easyNPC, name, color, visibility)) {
      return sendSuccessMessage(
          context,
          "Set name of "
              + easyNPC
              + " to "
              + name
              + " with color "
              + chatFormatting.name().toLowerCase()
              + " and visibility "
              + visibility.name().toLowerCase());
    }
    return 0;
  }

  private static int setNameColor(
      CommandSourceStack context, EasyNPC<?> easyNPC, String colorName) {
    if (easyNPC == null || colorName == null) {
      return 0;
    }

    Component currentName = easyNPC.getEntity().getCustomName();
    if (currentName == null) {
      return sendFailureMessage(context, "NPC has no custom name set. Use 'name set' first.");
    }

    ChatFormatting chatFormatting = parseChatFormatting(colorName);
    if (chatFormatting == null) {
      return sendFailureMessage(context, "Invalid color: " + colorName);
    }

    String nameText = currentName.getString();
    boolean isVisible = easyNPC.getEntity().isCustomNameVisible();
    NameVisibilityType visibility =
        isVisible ? NameVisibilityType.ALWAYS : NameVisibilityType.NEVER;

    int color = chatFormatting.getColor() != null ? chatFormatting.getColor() : -1;
    if (NameHandler.setCustomName(easyNPC, nameText, color, visibility)) {
      return sendSuccessMessage(
          context, "Set color of " + easyNPC + " name to " + chatFormatting.name().toLowerCase());
    }
    return 0;
  }

  private static int setNameVisibility(
      CommandSourceStack context, EasyNPC<?> easyNPC, String visibilityName) {
    if (easyNPC == null || visibilityName == null) {
      return 0;
    }

    Component currentName = easyNPC.getEntity().getCustomName();
    if (currentName == null) {
      return sendFailureMessage(context, "NPC has no custom name set. Use 'name set' first.");
    }

    NameVisibilityType visibility = parseNameVisibilityType(visibilityName);
    if (visibility == null) {
      return sendFailureMessage(context, "Invalid visibility: " + visibilityName);
    }

    String nameText = currentName.getString();
    if (NameHandler.setCustomName(easyNPC, nameText, -1, visibility)) {
      return sendSuccessMessage(
          context, "Set visibility of " + easyNPC + " name to " + visibility.name().toLowerCase());
    }
    return 0;
  }

  private static int clearName(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return 0;
    }

    if (NameHandler.setCustomName(easyNPC, "", -1, NameVisibilityType.NEVER)) {
      return sendSuccessMessage(context, "Cleared name of " + easyNPC);
    }
    return 0;
  }

  private static ChatFormatting parseChatFormatting(String colorName) {
    try {
      return ChatFormatting.valueOf(colorName.toUpperCase());
    } catch (IllegalArgumentException e) {
      return null;
    }
  }

  private static NameVisibilityType parseNameVisibilityType(String visibilityName) {
    try {
      return NameVisibilityType.valueOf(visibilityName.toUpperCase());
    } catch (IllegalArgumentException e) {
      return null;
    }
  }
}
