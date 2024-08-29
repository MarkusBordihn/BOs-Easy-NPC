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
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.menu.ClientMenuManager;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;

public record OpenMenuCallbackMessage(UUID uuid, UUID menuId, CompoundTag data)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "open_menu_callback_message");
  public static final CustomPacketPayload.Type<OpenMenuCallbackMessage> PAYLOAD_TYPE =
      new CustomPacketPayload.Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, OpenMenuCallbackMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), OpenMenuCallbackMessage::create);

  public static OpenMenuCallbackMessage create(final FriendlyByteBuf buffer) {
    return new OpenMenuCallbackMessage(buffer.readUUID(), buffer.readUUID(), buffer.readNbt());
  }

  @Override
  public void write(FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUUID(this.menuId);
    buffer.writeNbt(this.data);
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<OpenMenuCallbackMessage> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleClient() {
    UUID uuid = this.uuid;
    UUID menuId = this.menuId;
    CompoundTag data = this.data;

    // Validate menu data
    if (uuid == null || menuId == null || data == null) {
      log.error(
          "Invalid menu data received for {} with menuId {} and data: {}", uuid, menuId, data);
      return;
    }

    // Update menu data within the client menu manager
    ClientMenuManager.setMenuData(menuId, data);

    // Check if additional screen data is available and re-use some of the data.
    if (ClientMenuManager.hasAdditionalScreenData()) {
      AdditionalScreenData additionalScreenData = ClientMenuManager.getAdditionalScreenData();

      // Store dialog data set if available.
      if (additionalScreenData.hasDialogDataSet()) {
        DialogDataManager.addDialogDataSet(this.uuid, additionalScreenData.getDialogDataSet());
      }
    }

    // Request to open the menu on the client side over the server.
    NetworkMessageHandlerManager.getServerHandler().openMenu(uuid, menuId);
  }
}
