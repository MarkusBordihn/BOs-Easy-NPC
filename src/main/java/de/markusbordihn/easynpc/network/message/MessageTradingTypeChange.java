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

import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;

public class MessageTradingTypeChange extends NetworkMessage {

  protected final TradingType tradingType;

  public MessageTradingTypeChange(UUID uuid, TradingType tradingType) {
    super(uuid);
    this.tradingType = tradingType;
  }

  public TradingType getTradingType() {
    return this.tradingType;
  }

  public static MessageTradingTypeChange decode(FriendlyByteBuf buffer) {
    return new MessageTradingTypeChange(buffer.readUUID(), buffer.readEnum(TradingType.class));
  }

  public static void encode(MessageTradingTypeChange message, FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getTradingType());
  }

  public static void handle(MessageTradingTypeChange message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageTradingTypeChange message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    TradingType tradingType = message.getTradingType();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || tradingType == null
        || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      log.error("Unable to change trading type with message {} from {}", message, context);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    log.debug("Change trading type: {} for {} from {}", tradingType, easyNPCEntity, serverPlayer);
    easyNPCEntity.setTradingType(tradingType);
  }

}
