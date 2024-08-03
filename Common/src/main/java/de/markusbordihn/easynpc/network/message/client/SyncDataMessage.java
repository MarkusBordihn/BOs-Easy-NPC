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

package de.markusbordihn.easynpc.network.message.client;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.dialog.DialogDataManager;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;

public record SyncDataMessage(UUID uuid, DialogDataSet dialogDataSet)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "sync_data");
  public static final CustomPacketPayload.Type<SyncDataMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, SyncDataMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), SyncDataMessage::create);

  public static SyncDataMessage create(final FriendlyByteBuf buffer) {
    return new SyncDataMessage(buffer.readUUID(), new DialogDataSet(buffer.readNbt()));
  }

  @Override
  public void write(FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeNbt(this.dialogDataSet.createTag());
  }

  @Override
  public Type<SyncDataMessage> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public void handleClient() {
    if (this.uuid == null || this.uuid.toString().isEmpty()) {
      log.error("Invalid UUID {} for {}", this.uuid, this);
      return;
    }

    if (this.dialogDataSet != null) {
      log.debug("Syncing dialog data for {} with {}", this.uuid, this.dialogDataSet);
      DialogDataManager.addDialogDataSet(this.uuid, this.dialogDataSet);
    }
  }
}
