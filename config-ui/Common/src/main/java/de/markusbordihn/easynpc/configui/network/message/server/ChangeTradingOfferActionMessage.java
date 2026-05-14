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

package de.markusbordihn.easynpc.configui.network.message.server;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.security.SecurityManager;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;

public record ChangeTradingOfferActionMessage(
    UUID uuid, int offerIndex, ActionDataSet actionDataSet) implements NetworkMessageRecord {

  public static final Identifier MESSAGE_ID =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "change_trading_offer_action");
  public static final Type<ChangeTradingOfferActionMessage> PAYLOAD_TYPE = new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeTradingOfferActionMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ChangeTradingOfferActionMessage::create);

  public static ChangeTradingOfferActionMessage create(final FriendlyByteBuf buffer) {
    return new ChangeTradingOfferActionMessage(
        buffer.readUUID(), buffer.readInt(), new ActionDataSet(buffer.readNbt()));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeInt(this.offerIndex);
    buffer.writeNbt(this.actionDataSet.createTag());
  }

  @Override
  public Identifier id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<? extends CustomPacketPayload> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null || this.offerIndex < 0 || this.actionDataSet == null) {
      return;
    }

    TradingDataCapable<?> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData == null) {
      log.error("No TradingDataCapable for {} from {}", easyNPC, serverPlayer);
      return;
    }

    CommandPermissionLevel permissionLevel =
        SecurityManager.applyActionAuthority(easyNPC, serverPlayer);
    ActionDataSet sanitizedDataSet =
        MessageSecurity.sanitizeActionDataSet(
            this.actionDataSet, easyNPC, serverPlayer, permissionLevel);
    if (sanitizedDataSet == null) {
      log.warn(
          "Blocked trading offer action change for offer {} of {} from {} because it contains an action type blocked by security.cfg feature settings",
          offerIndex,
          easyNPC,
          serverPlayer);
      return;
    }

    tradingData.getTradingDataSet().setOfferAction(this.offerIndex, sanitizedDataSet);
  }
}
