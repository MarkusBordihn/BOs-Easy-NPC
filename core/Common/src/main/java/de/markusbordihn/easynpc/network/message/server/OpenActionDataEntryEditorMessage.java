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
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record OpenActionDataEntryEditorMessage(
    UUID uuid,
    UUID dialogId,
    UUID dialogButtonId,
    UUID actionDataEntryId,
    ActionEventType actionEventType,
    ConfigurationType configurationType,
    EditorType editorType)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "open_action_data_entry_editor");
  public static final CustomPacketPayload.Type<OpenActionDataEntryEditorMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, OpenActionDataEntryEditorMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), OpenActionDataEntryEditorMessage::create);

  public static OpenActionDataEntryEditorMessage create(final FriendlyByteBuf buffer) {
    return new OpenActionDataEntryEditorMessage(
        buffer.readUUID(),
        buffer.readUUID(),
        buffer.readUUID(),
        buffer.readUUID(),
        buffer.readEnum(ActionEventType.class),
        buffer.readEnum(ConfigurationType.class),
        buffer.readEnum(EditorType.class));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUUID(this.dialogId);
    buffer.writeUUID(this.dialogButtonId);
    buffer.writeUUID(this.actionDataEntryId);
    buffer.writeEnum(this.actionEventType);
    buffer.writeEnum(this.configurationType);
    buffer.writeEnum(this.editorType);
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

    // Open action data entry editor
    MenuManager.getMenuHandler()
        .openEditorMenu(
            EditorType.ACTION_DATA_ENTRY,
            serverPlayer,
            easyNPC,
            this.dialogId,
            this.dialogButtonId,
            this.actionDataEntryId,
            this.actionEventType,
            this.configurationType,
            this.editorType,
            0);
  }
}
