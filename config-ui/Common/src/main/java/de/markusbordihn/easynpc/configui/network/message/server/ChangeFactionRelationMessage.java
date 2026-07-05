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
import de.markusbordihn.easynpc.data.saveddata.FactionData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.security.NpcFeature;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;

public record ChangeFactionRelationMessage(
    UUID uuid, String factionName, String targetFactionName, boolean hostile, boolean mutual)
    implements NetworkMessageRecord {

  public static final Identifier MESSAGE_ID =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "change_faction_relation");
  public static final Type<ChangeFactionRelationMessage> PAYLOAD_TYPE = new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeFactionRelationMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ChangeFactionRelationMessage::create);

  public static ChangeFactionRelationMessage create(final FriendlyByteBuf buffer) {
    return new ChangeFactionRelationMessage(
        buffer.readUUID(),
        buffer.readUtf(),
        buffer.readUtf(),
        buffer.readBoolean(),
        buffer.readBoolean());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUtf(this.factionName != null ? this.factionName : "");
    buffer.writeUtf(this.targetFactionName != null ? this.targetFactionName : "");
    buffer.writeBoolean(this.hostile);
    buffer.writeBoolean(this.mutual);
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
    if (easyNPC == null
        || this.factionName == null
        || this.factionName.isEmpty()
        || this.targetFactionName == null
        || this.targetFactionName.isEmpty()
        || this.factionName.equals(this.targetFactionName)) {
      log.error("Invalid data to change faction relation for {}: ", this);
      return;
    }

    if (!MessageSecurity.checkFeatureAccess(
        serverPlayer, easyNPC, NpcFeature.FACTION_MANAGEMENT, "change faction relation")) {
      return;
    }

    if (!FactionData.isInitialized()
        || !FactionData.get().hasFaction(this.factionName)
        || !FactionData.get().hasFaction(this.targetFactionName)) {
      log.error(
          "Unknown faction '{}' or '{}' to change faction relation for {}",
          this.factionName,
          this.targetFactionName,
          easyNPC);
      return;
    }

    FactionData factionData = FactionData.get();
    if (this.hostile) {
      factionData.addHostileFaction(this.factionName, this.targetFactionName);
      if (this.mutual) {
        factionData.addHostileFaction(this.targetFactionName, this.factionName);
      }
    } else {
      factionData.removeHostileFaction(this.factionName, this.targetFactionName);
      if (this.mutual) {
        factionData.removeHostileFaction(this.targetFactionName, this.factionName);
      }
    }

    new OpenFactionEditorMessage(this.uuid, this.factionName).handleServer(serverPlayer);
  }
}
