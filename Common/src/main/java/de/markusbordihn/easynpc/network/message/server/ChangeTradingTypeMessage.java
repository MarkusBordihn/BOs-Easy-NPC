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

package de.markusbordihn.easynpc.network.message.server;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ChangeTradingTypeMessage extends NetworkMessage<ChangeTradingTypeMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_trading_type");

  protected final TradingType tradingType;

  public ChangeTradingTypeMessage(final UUID uuid, final TradingType tradingType) {
    super(uuid);
    this.tradingType = tradingType;
  }

  public static ChangeTradingTypeMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeTradingTypeMessage(buffer.readUUID(), buffer.readEnum(TradingType.class));
  }

  public static FriendlyByteBuf encode(
      final ChangeTradingTypeMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getTradingType());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(
      final ChangeTradingTypeMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate trading type
    TradingType tradingType = message.getTradingType();
    if (tradingType == null) {
      log.error("Invalid trading type for {} from {}", message, serverPlayer);
      return;
    }

    // Validate trading data.
    EasyNPC<?> easyNPC = message.getEasyNPC();
    TradingData<?> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData == null) {
      log.error("Invalid trading data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    log.debug("Change trading type: {} for {} from {}", tradingType, easyNPC, serverPlayer);
    tradingData.setTradingType(tradingType);
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public ChangeTradingTypeMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public TradingType getTradingType() {
    return this.tradingType;
  }
}
