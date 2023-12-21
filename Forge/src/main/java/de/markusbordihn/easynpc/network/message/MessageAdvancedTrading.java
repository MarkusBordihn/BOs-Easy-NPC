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

package de.markusbordihn.easynpc.network.message;

import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessageAdvancedTrading extends NetworkMessage {

  protected final int tradingOfferIndex;
  protected final float tradingValue;
  protected final TradingValueType tradingValueType;

  public MessageAdvancedTrading(
      UUID uuid, int tradingOfferIndex, TradingValueType tradingValueType, float tradingValue) {
    super(uuid);
    this.tradingOfferIndex = tradingOfferIndex;
    this.tradingValueType = tradingValueType;
    this.tradingValue = tradingValue;
  }

  public static MessageAdvancedTrading decode(final FriendlyByteBuf buffer) {
    return new MessageAdvancedTrading(
        buffer.readUUID(),
        buffer.readInt(),
        buffer.readEnum(TradingValueType.class),
        buffer.readFloat());
  }

  public static void encode(final MessageAdvancedTrading message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeInt(message.getTradingOfferIndex());
    buffer.writeEnum(message.getTradingValueType());
    buffer.writeFloat(message.getTradingValue());
  }

  public static void handle(MessageAdvancedTrading message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageAdvancedTrading message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate trading offer index
    int tradingOfferIndex = message.getTradingOfferIndex();
    if (tradingOfferIndex < 0) {
      log.error(
          "Trading offer index {} is out of range (>= 0) for {}", tradingOfferIndex, serverPlayer);
      return;
    }

    // Validate trading value type
    TradingValueType tradingValueType = message.getTradingValueType();
    if (tradingValueType == null) {
      log.error("Trading value type {} is unknown for {}", tradingValueType, serverPlayer);
      return;
    }

    // Validate trading value
    float tradingValue = message.getTradingValue();
    if (tradingValue < 0.0) {
      log.error(
          "Trading value {} for {} is out of range (>= 0) for {}",
          tradingValue,
          tradingValueType,
          serverPlayer);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    switch (tradingValueType) {
      case RESETS_EVERY_MIN:
        log.debug(
            "Set trading resets every min to {} for {} from {}",
            tradingValue,
            easyNPCEntity,
            serverPlayer);
        easyNPCEntity.setTradingResetsEveryMin((int) tradingValue);
        break;
      case MAX_USES:
        log.debug(
            "Set advanced trading max uses {}# for {} to {} by {}",
            tradingOfferIndex,
            easyNPCEntity,
            tradingValue,
            serverPlayer);
        easyNPCEntity.setAdvancedTradingMaxUses(tradingOfferIndex, (int) tradingValue);
        break;
      case XP:
        log.debug(
            "Set advanced trading xp {}# for {} to {} by {}",
            tradingOfferIndex,
            easyNPCEntity,
            tradingValue,
            serverPlayer);
        easyNPCEntity.setAdvancedTradingXp(tradingOfferIndex, (int) tradingValue);
        break;
      case PRICE_MULTIPLIER:
        log.debug(
            "Set advanced trading price multiplier {}# for {} to {} by {}",
            tradingOfferIndex,
            easyNPCEntity,
            tradingValue,
            serverPlayer);
        easyNPCEntity.setAdvancedTradingPriceMultiplier(tradingOfferIndex, tradingValue);
        break;
      case DEMAND:
        log.debug(
            "Set advanced trading demand {}# for {} to {} by {}",
            tradingOfferIndex,
            easyNPCEntity,
            tradingValue,
            serverPlayer);
        easyNPCEntity.setAdvancedTradingDemand(tradingOfferIndex, (int) tradingValue);
        break;
      default:
        log.error(
            "Trading value type {} with value {}# for {} is unknown for {}",
            tradingValueType,
            tradingValue,
            tradingOfferIndex,
            serverPlayer);
    }
  }

  public int getTradingOfferIndex() {
    return this.tradingOfferIndex;
  }

  public TradingValueType getTradingValueType() {
    return this.tradingValueType;
  }

  public float getTradingValue() {
    return this.tradingValue;
  }

  public enum TradingValueType {
    RESETS_EVERY_MIN,
    MAX_USES,
    XP,
    PRICE_MULTIPLIER,
    DEMAND
  }
}
