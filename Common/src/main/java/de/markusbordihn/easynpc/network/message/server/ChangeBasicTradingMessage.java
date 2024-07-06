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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ChangeBasicTradingMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_basic_trading");

  protected final int tradingValue;
  protected final TradingValueType tradingValueType;

  public ChangeBasicTradingMessage(
      final UUID uuid, final TradingValueType tradingValueType, final int tradingValue) {
    super(uuid);
    this.tradingValueType = tradingValueType;
    this.tradingValue = tradingValue;
  }

  public static ChangeBasicTradingMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeBasicTradingMessage(
        buffer.readUUID(), buffer.readEnum(TradingValueType.class), buffer.readInt());
  }

  public static FriendlyByteBuf encode(
      final ChangeBasicTradingMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getTradingValueType());
    buffer.writeInt(message.getTradingValue());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(
      final ChangeBasicTradingMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate trading value type
    TradingValueType tradingValueType = message.getTradingValueType();
    if (tradingValueType == null) {
      log.error("Trading value type is unknown for {} from {}", message, serverPlayer);
      return;
    }

    // Validate trading value
    int tradingValue = message.getTradingValue();
    if (tradingValue < 0) {
      log.error(
          "Trading value {} for {} is out of range (>= 0) for {}",
          tradingValue,
          tradingValueType,
          serverPlayer);
      return;
    }

    // Validate trading data
    EasyNPC<?> easyNPC = message.getEasyNPC();
    TradingData<?> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData == null) {
      log.error("Trading data for {} is not available for {}", easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    switch (tradingValueType) {
      case RESETS_EVERY_MIN:
        log.debug(
            "Set trading resets every min to {} for {} from {}",
            tradingValue,
            easyNPC,
            serverPlayer);
        tradingData.setTradingResetsEveryMin(tradingValue);
        break;
      case MAX_USES:
        log.debug("Set max uses to {} for {} from {}", tradingValue, easyNPC, serverPlayer);
        tradingData.setBasicTradingMaxUses(tradingValue);
        tradingData.updateBasicTradingOffers();
        break;
      case REWARD_EXP:
        log.debug("Set reward exp to {} for {} from {}", tradingValue, easyNPC, serverPlayer);
        tradingData.setBasicTradingRewardExp(tradingValue);
        tradingData.updateBasicTradingOffers();
        break;
      default:
        log.error("Trading value type {} is unknown for {}", tradingValueType, serverPlayer);
    }
  }

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public TradingValueType getTradingValueType() {
    return this.tradingValueType;
  }

  public int getTradingValue() {
    return this.tradingValue;
  }

  public enum TradingValueType {
    RESETS_EVERY_MIN,
    MAX_USES,
    REWARD_EXP
  }
}
