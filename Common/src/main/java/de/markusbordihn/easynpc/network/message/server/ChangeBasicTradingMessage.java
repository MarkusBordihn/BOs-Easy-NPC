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
import de.markusbordihn.easynpc.data.trading.TradingValueType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ChangeBasicTradingMessage(
    UUID uuid, TradingValueType tradingValueType, int tradingValue)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "change_basic_trading");
  public static final CustomPacketPayload.Type<ChangeBasicTradingMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeBasicTradingMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), ChangeBasicTradingMessage::create);

  public static ChangeBasicTradingMessage create(final FriendlyByteBuf buffer) {
    return new ChangeBasicTradingMessage(
        buffer.readUUID(), buffer.readEnum(TradingValueType.class), buffer.readInt());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.tradingValueType);
    buffer.writeInt(this.tradingValue);
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<? extends CustomPacketPayload> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    // Validate trading value type
    if (this.tradingValueType == null) {
      log.error("Trading value type is unknown for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate trading value
    if (this.tradingValue < 0) {
      log.error(
          "Trading value {} for {} is out of range (>= 0) for {}",
          this.tradingValue,
          this.tradingValueType,
          serverPlayer);
      return;
    }

    // Validate trading data
    TradingData<?> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData == null) {
      log.error("Trading data for {} is not available for {}", easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    switch (this.tradingValueType) {
      case RESET_TRADING_EVERY_MIN:
        log.debug(
            "Set trading resets every min to {} for {} from {}",
            this.tradingValue,
            easyNPC,
            serverPlayer);
        tradingData.getTradingDataSet().setResetsEveryMin(this.tradingValue);
        break;
      case MAX_USES:
        log.debug("Set max uses to {} for {} from {}", this.tradingValue, easyNPC, serverPlayer);
        tradingData.getTradingDataSet().setMaxUses(this.tradingValue);
        tradingData.updateBasicTradingOffers();
        break;
      case REWARD_EXP:
        log.debug("Set reward exp to {} for {} from {}", this.tradingValue, easyNPC, serverPlayer);
        tradingData.getTradingDataSet().setRewardedXP(this.tradingValue);
        tradingData.updateBasicTradingOffers();
        break;
      default:
        log.error("Trading value type {} is unknown for {}", this.tradingValueType, serverPlayer);
    }
  }
}
