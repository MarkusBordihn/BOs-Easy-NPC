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

import com.mojang.brigadier.arguments.DoubleArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.attribute.BaseAttributeType;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import java.util.Collection;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.world.entity.LivingEntity;

public class AttributeCommand extends Command {

  private AttributeCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    RequiredArgumentBuilder<CommandSourceStack, ?> npcTargetsArgument =
        Commands.argument(NPC_TARGETS_ARG, EasyNPCArgument.npc());

    npcTargetsArgument.then(
        Commands.literal(DisplayAttributeType.LIGHT_LEVEL.getAttributeName())
            .then(
                Commands.argument(
                        VALUE_ARG,
                        IntegerArgumentType.integer(
                            DisplayAttributeType.MIN_LIGHT_LEVEL,
                            DisplayAttributeType.MAX_LIGHT_LEVEL))
                    .executes(
                        context ->
                            setLightLevel(
                                context.getSource(),
                                EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG),
                                IntegerArgumentType.getInteger(context, VALUE_ARG)))));

    for (BaseAttributeType baseAttributeType : BaseAttributeType.values()) {
      npcTargetsArgument.then(
          Commands.literal(baseAttributeType.getAttributeName())
              .then(
                  // AttributeFix on Fabric changes the ranges on server start, after command setup.
                  Commands.argument(VALUE_ARG, DoubleArgumentType.doubleArg())
                      .executes(
                          context ->
                              setBaseAttribute(
                                  context.getSource(),
                                  EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG),
                                  baseAttributeType,
                                  DoubleArgumentType.getDouble(context, VALUE_ARG)))));
    }

    return Commands.literal("attribute")
        .requires(
            commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(npcTargetsArgument);
  }

  private static int setLightLevel(
      CommandSourceStack context, Collection<? extends EasyNPC<?>> easyNPCs, int lightLevel) {
    int changedEntities = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      if (AttributeHandler.setDisplayAttribute(
          easyNPC, DisplayAttributeType.LIGHT_LEVEL, lightLevel)) {
        changedEntities++;
      }
    }

    return sendAttributeFeedback(
        context,
        easyNPCs,
        changedEntities,
        DisplayAttributeType.LIGHT_LEVEL.getAttributeName(),
        String.valueOf(lightLevel));
  }

  private static int setBaseAttribute(
      CommandSourceStack context,
      Collection<? extends EasyNPC<?>> easyNPCs,
      BaseAttributeType baseAttributeType,
      double value) {
    if (!baseAttributeType.isValidValue(value)) {
      return sendFailureMessage(
          context,
          "Value "
              + value
              + " for attribute "
              + baseAttributeType.getAttributeName()
              + " is out of range "
              + baseAttributeType.getMinValue()
              + " to "
              + baseAttributeType.getMaxValue()
              + "!");
    }

    int changedEntities = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      LivingEntity livingEntity = easyNPC.getLivingEntity();
      if (livingEntity == null
          || livingEntity.getAttribute(baseAttributeType.getAttribute()) == null) {
        sendFailureMessage(
            context,
            "Attribute "
                + baseAttributeType.getAttributeName()
                + " is not supported by NPC "
                + easyNPC.getEntityUUID()
                + ".");
        continue;
      }
      if (AttributeHandler.setBaseAttribute(
          easyNPC, baseAttributeType.getResourceLocation(), value)) {
        changedEntities++;
      }
    }

    return sendAttributeFeedback(
        context,
        easyNPCs,
        changedEntities,
        baseAttributeType.getAttributeName(),
        String.valueOf(value));
  }

  private static int sendAttributeFeedback(
      CommandSourceStack context,
      Collection<? extends EasyNPC<?>> easyNPCs,
      int changedEntities,
      String attributeName,
      String value) {
    if (changedEntities == 0) {
      return sendFailureMessage(context, "Attribute " + attributeName + " was not changed!");
    }

    return sendSuccessMessage(
        context,
        "Set attribute "
            + attributeName
            + " to "
            + value
            + " for "
            + changedEntities
            + " of "
            + easyNPCs.size()
            + " Easy NPCs!");
  }
}
