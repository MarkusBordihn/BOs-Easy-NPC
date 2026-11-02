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

import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BossBarHandler;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import java.util.Collection;
import java.util.Locale;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.world.BossEvent;

public class BossBarCommand extends Command {

  private BossBarCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    RequiredArgumentBuilder<CommandSourceStack, ?> npcTargetsArgument =
        Commands.argument(NPC_TARGETS_ARG, EasyNPCArgument.npc());

    npcTargetsArgument
        .then(
            Commands.literal("show")
                .executes(
                    context ->
                        setBossBarAttribute(
                            context,
                            "visibility",
                            "shown",
                            DisplayAttributeType.SHOW_BOSS_BAR,
                            true)))
        .then(
            Commands.literal("hide")
                .executes(
                    context ->
                        setBossBarAttribute(
                            context,
                            "visibility",
                            "hidden",
                            DisplayAttributeType.SHOW_BOSS_BAR,
                            false)))
        .then(colorLiteral())
        .then(styleLiteral())
        .then(
            integerSetting(
                "range",
                DisplayAttributeType.BOSS_BAR_RANGE,
                DisplayAttributeType.MIN_BOSS_BAR_RANGE,
                DisplayAttributeType.MAX_BOSS_BAR_RANGE))
        .then(
            integerSetting(
                "auto_hide",
                DisplayAttributeType.BOSS_BAR_AUTO_HIDE,
                DisplayAttributeType.MIN_BOSS_BAR_AUTO_HIDE_SECONDS,
                DisplayAttributeType.MAX_BOSS_BAR_AUTO_HIDE_SECONDS))
        .then(Commands.literal("info").executes(BossBarCommand::info));

    return Commands.literal("boss_bar")
        .requires(
            commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(npcTargetsArgument);
  }

  private static LiteralArgumentBuilder<CommandSourceStack> colorLiteral() {
    LiteralArgumentBuilder<CommandSourceStack> colorLiteral = Commands.literal("color");
    for (BossEvent.BossBarColor bossBarColor : BossEvent.BossBarColor.values()) {
      String colorName = bossBarColor.name().toLowerCase(Locale.ROOT);
      colorLiteral.then(
          Commands.literal(colorName)
              .executes(
                  context ->
                      setBossBarAttribute(
                          context,
                          "color",
                          colorName,
                          DisplayAttributeType.BOSS_BAR_COLOR,
                          bossBarColor.name())));
    }
    return colorLiteral;
  }

  private static LiteralArgumentBuilder<CommandSourceStack> styleLiteral() {
    LiteralArgumentBuilder<CommandSourceStack> styleLiteral = Commands.literal("style");
    for (BossEvent.BossBarOverlay bossBarStyle : BossEvent.BossBarOverlay.values()) {
      String styleName = bossBarStyle.name().toLowerCase(Locale.ROOT);
      styleLiteral.then(
          Commands.literal(styleName)
              .executes(
                  context ->
                      setBossBarAttribute(
                          context,
                          "style",
                          styleName,
                          DisplayAttributeType.BOSS_BAR_STYLE,
                          bossBarStyle.name())));
    }
    return styleLiteral;
  }

  private static ArgumentBuilder<CommandSourceStack, ?> integerSetting(
      String settingName, DisplayAttributeType attributeType, int minimum, int maximum) {
    return Commands.literal(settingName)
        .then(
            Commands.argument(VALUE_ARG, IntegerArgumentType.integer(minimum, maximum))
                .executes(
                    context -> {
                      int value = IntegerArgumentType.getInteger(context, VALUE_ARG);
                      return setBossBarAttribute(
                          context, settingName, String.valueOf(value), attributeType, value);
                    }));
  }

  private static <T> int setBossBarAttribute(
      CommandContext<CommandSourceStack> context,
      String settingName,
      String valueName,
      DisplayAttributeType attributeType,
      T value)
      throws CommandSyntaxException {
    Collection<? extends EasyNPC<?>> easyNPCs =
        EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG);
    int changedEntities = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      if (AttributeHandler.setDisplayAttribute(easyNPC, attributeType, value)) {
        changedEntities++;
      }
    }

    if (changedEntities == 0) {
      return sendFailureMessage(
          context.getSource(), "Boss bar " + settingName + " was not changed!");
    }

    return sendSuccessMessage(
        context.getSource(),
        "Set boss bar "
            + settingName
            + " to "
            + valueName
            + " for "
            + changedEntities
            + " of "
            + easyNPCs.size()
            + " Easy NPCs!");
  }

  private static int info(CommandContext<CommandSourceStack> context)
      throws CommandSyntaxException {
    Collection<? extends EasyNPC<?>> easyNPCs =
        EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG);
    int listedEntities = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      DisplayAttributeDataCapable<?> displayAttributeData =
          easyNPC.getEasyNPCDisplayAttributeData();
      if (displayAttributeData == null) {
        continue;
      }

      String visibility =
          displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.SHOW_BOSS_BAR)
              ? "shown"
              : "hidden";
      int autoHideSeconds = BossBarHandler.getBossBarAutoHideSeconds(displayAttributeData);
      String autoHide = autoHideSeconds == 0 ? "off" : autoHideSeconds + " s";
      sendSuccessMessage(
          context.getSource(),
          "Boss bar of "
              + easyNPC.getEntity().getDisplayName().getString()
              + ": "
              + visibility
              + ", color "
              + BossBarHandler.getBossBarColor(displayAttributeData).name().toLowerCase(Locale.ROOT)
              + ", style "
              + BossBarHandler.getBossBarStyle(displayAttributeData).name().toLowerCase(Locale.ROOT)
              + ", range "
              + BossBarHandler.getBossBarRange(displayAttributeData)
              + " blocks, auto hide "
              + autoHide);
      listedEntities++;
    }

    if (listedEntities == 0) {
      return sendFailureMessage(context.getSource(), "No boss bar data found!");
    }

    return listedEntities;
  }
}
