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
import de.markusbordihn.easynpc.menu.ClientMenuManager;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;

public class OpenMenuCallbackMessage extends NetworkMessage<OpenMenuCallbackMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "open_menu_callback_message");

  private final UUID menuId;
  private final CompoundTag data;

  public OpenMenuCallbackMessage(final UUID uuid, final UUID menuId) {
    this(uuid, menuId, new CompoundTag());
  }

  public OpenMenuCallbackMessage(final UUID uuid, final UUID menuId, CompoundTag data) {
    super(uuid);
    this.menuId = menuId;
    this.data = data;
  }

  public static OpenMenuCallbackMessage decode(final FriendlyByteBuf buffer) {
    return new OpenMenuCallbackMessage(buffer.readUUID(), buffer.readUUID(), buffer.readNbt());
  }

  public static FriendlyByteBuf encode(
      final OpenMenuCallbackMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getMenuId());
    buffer.writeNbt(message.getData());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer) {
    handle(decode(buffer));
  }

  public static void handle(final OpenMenuCallbackMessage message) {
    UUID uuid = message.getUUID();
    UUID menuId = message.getMenuId();
    CompoundTag data = message.getData();

    // Store additional menu data for later use.
    ClientMenuManager.setMenuData(menuId, data);

    // Send open menu message back to server for final processing.
    NetworkMessageHandlerManager.getServerHandler().openMenu(uuid, menuId);
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public OpenMenuCallbackMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public UUID getMenuId() {
    return this.menuId;
  }

  public CompoundTag getData() {
    return this.data;
  }
}
