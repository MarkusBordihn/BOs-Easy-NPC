/**
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

import java.util.UUID;
import java.util.function.Supplier;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;

import net.minecraftforge.network.NetworkEvent;

import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;

public class MessageBasicTrading extends NetworkMessage {

  protected final int tradingValue;
  protected final TradingValueType tradingValueType;

  public MessageBasicTrading(UUID uuid, TradingValueType tradingValueType, int tradingValue) {
    super(uuid);
    this.tradingValueType = tradingValueType;
    this.tradingValue = tradingValue;
  }

  public enum TradingValueType {
    RESETS_EVERY_MIN, MAX_USES, REWARD_EXP;
  }

  public TradingValueType getTradingValueType() {
    return this.tradingValueType;
  }

  public int getTradingValue() {
    return this.tradingValue;
  }

  public static MessageBasicTrading decode(final FriendlyByteBuf buffer) {
    return new MessageBasicTrading(buffer.readUUID(), buffer.readEnum(TradingValueType.class),
        buffer.readInt());
  }

  public static void encode(final MessageBasicTrading message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getTradingValueType());
    buffer.writeInt(message.getTradingValue());
  }

  public static void handle(MessageBasicTrading message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageBasicTrading message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate trading value type
    TradingValueType tradingValueType = message.getTradingValueType();
    if (tradingValueType == null) {
      log.error("Trading value type {} is unknown for {}", tradingValueType, serverPlayer);
      return;
    }

    // Validate trading value
    int tradingValue = message.getTradingValue();
    if (tradingValue < 0) {
      log.error("Trading value {} for {} is out of range (>= 0) for {}", tradingValue,
          tradingValueType, serverPlayer);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    switch (tradingValueType) {
      case RESETS_EVERY_MIN:
        log.debug("Set trading resets every min to {} for {} from {}", tradingValue, easyNPCEntity,
            serverPlayer);
        easyNPCEntity.setTradingResetsEveryMin(tradingValue);
        break;
      case MAX_USES:
        log.debug("Set max uses to {} for {} from {}", tradingValue, easyNPCEntity, serverPlayer);
        easyNPCEntity.setBasicTradingMaxUses(tradingValue);
        easyNPCEntity.updateBasicTradingOffers();
        break;
      case REWARD_EXP:
        log.debug("Set reward exp to {} for {} from {}", tradingValue, easyNPCEntity, serverPlayer);
        easyNPCEntity.setBasicTradingRewardExp(tradingValue);
        easyNPCEntity.updateBasicTradingOffers();
        break;
      default:
        log.error("Trading value type {} is unknown for {}", tradingValueType, serverPlayer);
        return;
    }
  }

}
