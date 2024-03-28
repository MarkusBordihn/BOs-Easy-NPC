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

package de.markusbordihn.easynpc.commands;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class OwnerCommand {
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String EASY_NPC_PREFIX = "EasyNPC ";

  public OwnerCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("owner")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(
            Commands.literal("set")
                .then(
                    Commands.argument("uuid", UuidArgument.uuid())
                        .suggests(SuggestionProvider::suggestEasyNPCs)
                        .then(
                            Commands.argument("player", EntityArgument.player())
                                .executes(
                                    context ->
                                        setOwner(
                                            context.getSource(),
                                            UuidArgument.getUuid(context, "uuid"),
                                            EntityArgument.getPlayer(context, "player"))))))
        .then(
            Commands.literal("get")
                .then(
                    Commands.argument("uuid", UuidArgument.uuid())
                        .suggests(SuggestionProvider::suggestEasyNPCs)
                        .executes(
                            context ->
                                getOwner(
                                    context.getSource(), UuidArgument.getUuid(context, "uuid")))));
  }

  private static int setOwner(
      CommandSourceStack context, @Nullable UUID uuid, ServerPlayer serverPlayer) {
    if (uuid == null || serverPlayer == null) {
      return 0;
    }
    log.debug("Try to set owner for EasyNPC with UUID {} to {} ...", uuid, serverPlayer);

    // Get the EasyNPC entity by UUID.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, context.getLevel());
    if (easyNPC == null) {
      context.sendFailure(new TextComponent("No EasyNPC found with UUID " + uuid));
      return 0;
    }

    // Get owner data for EasyNPC entity.
    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null) {
      context.sendFailure(new TextComponent("No owner data available for " + easyNPC));
      return 0;
    } else if (ownerData.getOwner() != null) {
      if (ownerData.getOwnerUUID().equals(serverPlayer.getUUID())) {
        context.sendSuccess(
            new TextComponent(
                EASY_NPC_PREFIX
                    + easyNPC.getUUID()
                    + " is already owned by "
                    + ownerData.getOwner()),
            false);
        return Command.SINGLE_SUCCESS;
      } else {
        context.sendFailure(
            new TextComponent(
                EASY_NPC_PREFIX
                    + easyNPC.getUUID()
                    + " is currently owned by "
                    + ownerData.getOwner()));
        return 0;
      }
    }

    // Set owner data for EasyNPC entity.
    ownerData.setOwnerUUID(serverPlayer.getUUID());
    context.sendSuccess(
        new TextComponent(
            EASY_NPC_PREFIX + easyNPC.getUUID() + " owners was changed to " + ownerData.getOwner()),
        false);
    return Command.SINGLE_SUCCESS;
  }

  private static int getOwner(CommandSourceStack context, UUID uuid) {
    if (uuid == null) {
      return 0;
    }
    log.debug("Try to get owner for EasyNPC with UUID {}...", uuid);

    // Get the EasyNPC entity by UUID.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, context.getLevel());
    if (easyNPC == null) {
      context.sendFailure(new TextComponent("No EasyNPC found with UUID " + uuid));
      return 0;
    }

    // Get owner data for EasyNPC entity.
    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null) {
      context.sendFailure(new TextComponent("No owner data available for " + easyNPC));
      return 0;
    }

    context.sendSuccess(
        new TextComponent(
            EASY_NPC_PREFIX + easyNPC.getUUID() + " is owned by " + ownerData.getOwner()),
        false);
    return Command.SINGLE_SUCCESS;
  }
}
