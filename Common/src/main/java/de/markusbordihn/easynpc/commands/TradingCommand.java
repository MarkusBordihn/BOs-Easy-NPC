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
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.trading.Merchant;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TradingCommand {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("trading")
        .then(
            Commands.literal("open")
                .requires(
                    commandSourceStack ->
                        commandSourceStack.hasPermission(Commands.LEVEL_GAMEMASTERS))
                .then(
                    Commands.argument("uuid", UuidArgument.uuid())
                        .suggests(SuggestionProvider::suggestEasyNPCs)
                        .executes(
                            context -> {
                              ServerPlayer serverPlayer =
                                  context.getSource().getPlayerOrException();
                              return open(
                                  context.getSource(),
                                  UuidArgument.getUuid(context, "uuid"),
                                  serverPlayer);
                            })
                        .then(
                            Commands.argument("player", EntityArgument.player())
                                .executes(
                                    context ->
                                        open(
                                            context.getSource(),
                                            UuidArgument.getUuid(context, "uuid"),
                                            EntityArgument.getPlayer(context, "player"))))))
        .then(
            Commands.literal("reset")
                .requires(
                    commandSourceStack ->
                        commandSourceStack.hasPermission(Commands.LEVEL_GAMEMASTERS))
                .then(
                    Commands.argument("uuid", UuidArgument.uuid())
                        .suggests(SuggestionProvider::suggestEasyNPCs)
                        .executes(
                            context ->
                                reset(
                                    context.getSource(), UuidArgument.getUuid(context, "uuid")))));
  }

  private static int reset(CommandSourceStack context, UUID uuid) {
    if (uuid == null) {
      return 0;
    }

    // Check if server player has access to the EasyNPC entity.
    if (!AccessManager.hasAccess(context, uuid)) {
      context.sendFailure(Component.literal("You are not allowed to edit this EasyNPC!"));
      return 0;
    }

    // Check for trading data
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, context.getLevel());
    TradingData<?> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData == null) {
      context.sendFailure(Component.literal("No trading data available for " + easyNPC));
      return 0;
    }

    // Reset trading offers for the EasyNPC entity.
    context.sendSuccess(() -> Component.literal("Resetting trading offers for " + easyNPC), false);
    tradingData.resetTradingOffers();
    return Command.SINGLE_SUCCESS;
  }

  private static int open(CommandSourceStack context, UUID uuid, ServerPlayer serverPlayer) {
    if (uuid == null || serverPlayer == null) {
      return 0;
    }

    // Try to get the EasyNPC entity by UUID.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      context.sendFailure(Component.literal("EasyNPC with UUID " + uuid + " not found!"));
      return 0;
    }

    // Verify merchant.
    Merchant merchant = easyNPC.getMerchant();
    if (merchant == null) {
      context.sendFailure(Component.literal("EasyNPC with UUID " + uuid + " has no merchant!"));
      return 0;
    }

    // Verify trading data.
    TradingData<?> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData == null) {
      context.sendFailure(Component.literal("EasyNPC with UUID " + uuid + " has no trading data!"));
      return 0;
    }

    // Open trading screen for the EasyNPC entity.
    tradingData.openTradingScreen(serverPlayer);

    return Command.SINGLE_SUCCESS;
  }
}
