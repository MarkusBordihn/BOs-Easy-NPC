/*
 * Copyright 2026 Markus Bordihn
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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.FactionHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.security.NpcFeature;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record RemoveFactionEntryMessage(UUID uuid, String factionName)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "remove_faction_entry");
  public static final Type<RemoveFactionEntryMessage> PAYLOAD_TYPE = new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, RemoveFactionEntryMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), RemoveFactionEntryMessage::create);

  public static RemoveFactionEntryMessage create(final FriendlyByteBuf buffer) {
    return new RemoveFactionEntryMessage(buffer.readUUID(), buffer.readUtf());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUtf(this.factionName != null ? this.factionName : "");
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
    if (easyNPC == null || this.factionName == null || this.factionName.isEmpty()) {
      log.error("Invalid data to remove faction for {}: ", this);
      return;
    }

    if (!MessageSecurity.checkFeatureAccess(
        serverPlayer, easyNPC, NpcFeature.FACTION_MANAGEMENT, "remove faction")) {
      return;
    }

    if (!FactionHandler.deleteFaction(this.factionName)) {
      log.error("Failed to remove faction '{}' for {}", this.factionName, easyNPC);
      return;
    }

    log.info("Removed faction '{}' for {} from {}", this.factionName, easyNPC, serverPlayer);
    new OpenFactionsEditorMessage(this.uuid).handleServer(serverPlayer);
  }
}
