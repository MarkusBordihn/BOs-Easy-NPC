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

import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.trading.Merchant;

public class TradingCommand extends Command {

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("trading")
        .then(
            Commands.literal("open")
                .requires(
                    commandSourceStack ->
                        commandSourceStack.hasPermission(Commands.LEVEL_GAMEMASTERS))
                .then(
                    Commands.argument("target", EasyNPCArgument.npc())
                        .executes(
                            context -> {
                              ServerPlayer serverPlayer =
                                  context.getSource().getPlayerOrException();
                              return open(
                                  context.getSource(),
                                  EasyNPCArgument.getEntityWithAccess(context, "target"),
                                  serverPlayer);
                            })
                        .then(
                            Commands.argument("player", EntityArgument.player())
                                .executes(
                                    context ->
                                        open(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(context, "target"),
                                            EntityArgument.getPlayer(context, "player"))))))
        .then(
            Commands.literal("reset")
                .requires(
                    commandSourceStack ->
                        commandSourceStack.hasPermission(Commands.LEVEL_GAMEMASTERS))
                .then(
                    Commands.argument("target", EasyNPCArgument.npc())
                        .executes(
                            context ->
                                reset(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, "target")))));
  }

  private static int reset(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return 0;
    }

    // Check for trading data
    TradingData<?> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData == null) {
      return sendFailureMessageNoTradingData(context, easyNPC);
    }

    // Reset trading offers for the EasyNPC entity.
    tradingData.resetTradingOffers();
    return sendSuccessMessage(
        context, "Trading offers for " + easyNPC + " were reset to default values.");
  }

  private static int open(
      CommandSourceStack context, EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    if (easyNPC == null || serverPlayer == null) {
      return 0;
    }

    // Verify merchant.
    Merchant merchant = easyNPC.getMerchant();
    if (merchant == null) {
      return sendFailureMessageNoMerchant(context, easyNPC);
    }

    // Verify trading data.
    TradingData<?> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData == null) {
      return sendFailureMessageNoTradingData(context, easyNPC);
    }

    // Open trading screen for the EasyNPC entity.
    tradingData.openTradingScreen(serverPlayer);
    return sendSuccessMessage(
        context, "Opened trading screen for " + serverPlayer + " with merchant " + easyNPC + " !");
  }
}
