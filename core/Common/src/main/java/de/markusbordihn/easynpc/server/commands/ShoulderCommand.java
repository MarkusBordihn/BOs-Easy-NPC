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
import de.markusbordihn.easynpc.api.handler.EasyNPCAttributeHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCShoulderHandler;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributes;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Locale;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class ShoulderCommand extends Command {

  private static final String COOLDOWN_TICKS_ARG = "cooldownTicks";

  private ShoulderCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("shoulder")
        .requires(commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("sit")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.argument(PLAYER_ARG, EntityArgument.player())
                                .executes(
                                    context ->
                                        sit(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            EntityArgument.getPlayer(context, PLAYER_ARG))))))
        .then(
            Commands.literal("dismount")
                .then(
                    Commands.argument(PLAYER_ARG, EntityArgument.player())
                        .executes(
                            context ->
                                dismount(
                                    context.getSource(),
                                    EntityArgument.getPlayer(context, PLAYER_ARG)))
                        .then(registerDismountSide(ShoulderSide.LEFT))
                        .then(registerDismountSide(ShoulderSide.RIGHT))))
        .then(
            Commands.literal("info")
                .then(
                    Commands.argument(PLAYER_ARG, EntityArgument.player())
                        .executes(
                            context ->
                                info(
                                    context.getSource(),
                                    EntityArgument.getPlayer(context, PLAYER_ARG)))))
        .then(
            Commands.literal("enable")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                setEnabled(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    true))))
        .then(
            Commands.literal("disable")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                setEnabled(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    false))))
        .then(registerOffset());
  }

  private static ArgumentBuilder<CommandSourceStack, ?> registerOffset() {
    DoubleArgumentType offsetArgument =
        DoubleArgumentType.doubleArg(
            -InteractionAttributes.MAX_SHOULDER_OFFSET, InteractionAttributes.MAX_SHOULDER_OFFSET);
    return Commands.literal("offset")
        .then(
            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                .then(
                    Commands.literal("reset")
                        .executes(
                            context ->
                                setOffset(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    Vec3.ZERO)))
                .then(
                    Commands.argument(X_ARG, offsetArgument)
                        .then(
                            Commands.argument(Y_ARG, offsetArgument)
                                .then(
                                    Commands.argument(Z_ARG, offsetArgument)
                                        .executes(
                                            context ->
                                                setOffset(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    new Vec3(
                                                        DoubleArgumentType.getDouble(
                                                            context, X_ARG),
                                                        DoubleArgumentType.getDouble(
                                                            context, Y_ARG),
                                                        DoubleArgumentType.getDouble(
                                                            context, Z_ARG))))))));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> registerDismountSide(
      ShoulderSide shoulderSide) {
    return Commands.literal(shoulderSide.name().toLowerCase(Locale.ROOT))
        .executes(
            context ->
                dismount(
                    context.getSource(),
                    EntityArgument.getPlayer(context, PLAYER_ARG),
                    shoulderSide,
                    0))
        .then(
            Commands.argument(COOLDOWN_TICKS_ARG, IntegerArgumentType.integer(0))
                .executes(
                    context ->
                        dismount(
                            context.getSource(),
                            EntityArgument.getPlayer(context, PLAYER_ARG),
                            shoulderSide,
                            IntegerArgumentType.getInteger(context, COOLDOWN_TICKS_ARG))));
  }

  private static int sit(CommandSourceStack context, EasyNPC<?> easyNPC, ServerPlayer player) {
    if (!canControlPlayer(context, player)) {
      return sendFailureMessageNotAllowed(context, player);
    }

    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, player)) {
      return sendFailureMessage(
          context,
          "Unable to put " + easyNPC + " on a shoulder of " + player.getName().getString() + "!");
    }

    return sendSuccessMessage(
        context, "Put " + easyNPC + " on a shoulder of " + player.getName().getString() + " !");
  }

  private static int dismount(CommandSourceStack context, ServerPlayer player) {
    if (!canControlPlayer(context, player)) {
      return sendFailureMessageNotAllowed(context, player);
    }

    if (!EasyNPCShoulderHandler.dismount(player)) {
      return sendFailureMessage(
          context, "Unable to put any Easy NPC down from " + player.getName().getString() + "!");
    }

    return sendSuccessMessage(
        context, "Put Easy NPCs down from " + player.getName().getString() + " !");
  }

  private static int dismount(
      CommandSourceStack context,
      ServerPlayer player,
      ShoulderSide shoulderSide,
      int remountCooldownTicks) {
    if (!canControlPlayer(context, player)) {
      return sendFailureMessageNotAllowed(context, player);
    }

    if (!EasyNPCShoulderHandler.dismount(player, shoulderSide, remountCooldownTicks)) {
      return sendFailureMessage(
          context,
          "Unable to put the Easy NPC on the "
              + shoulderSide
              + " shoulder of "
              + player.getName().getString()
              + " down!");
    }

    return sendSuccessMessage(
        context,
        "Put the Easy NPC on the "
            + shoulderSide
            + " shoulder of "
            + player.getName().getString()
            + " down with a remount cooldown of "
            + remountCooldownTicks
            + " ticks!");
  }

  private static int info(CommandSourceStack context, ServerPlayer player) {
    if (!canControlPlayer(context, player)) {
      return sendFailureMessageNotAllowed(context, player);
    }

    sendSuccessMessage(context, "Easy NPCs on the shoulders of " + player.getName().getString());
    for (ShoulderSide shoulderSide : ShoulderSide.values()) {
      sendSuccessMessage(
          context,
          "> "
              + shoulderSide
              + ": "
              + EasyNPCShoulderHandler.getShoulderNPCUUID(player, shoulderSide)
                  .map(UUID::toString)
                  .orElse("-"));
    }
    return SINGLE_SUCCESS;
  }

  private static int setEnabled(CommandSourceStack context, EasyNPC<?> easyNPC, boolean enabled) {
    if (!EasyNPCAttributeHandler.setInteractionAttribute(
        easyNPC, InteractionAttributeType.CAN_SIT_ON_SHOULDER, enabled)) {
      return sendFailureMessage(
          context, "Unable to change sitting on shoulders for " + easyNPC + "!");
    }

    return sendSuccessMessage(
        context,
        (enabled ? "Enabled" : "Disabled") + " sitting on shoulders for " + easyNPC + " !");
  }

  private static int setOffset(
      CommandSourceStack context, EasyNPC<?> easyNPC, Vec3 shoulderOffset) {
    if (!EasyNPCAttributeHandler.setShoulderOffset(easyNPC, shoulderOffset)) {
      return sendFailureMessage(context, "Unable to change the shoulder offset of " + easyNPC + "!");
    }

    return sendSuccessMessage(
        context, "Changed the shoulder offset of " + easyNPC + " to " + shoulderOffset + " !");
  }

  private static boolean canControlPlayer(CommandSourceStack context, ServerPlayer player) {
    return context.hasPermission(Commands.LEVEL_GAMEMASTERS) || player.equals(context.getEntity());
  }

  private static int sendFailureMessageNotAllowed(
      CommandSourceStack context, ServerPlayer player) {
    return sendFailureMessage(
        context,
        "You are not allowed to control the shoulders of " + player.getName().getString() + "!");
  }
}
