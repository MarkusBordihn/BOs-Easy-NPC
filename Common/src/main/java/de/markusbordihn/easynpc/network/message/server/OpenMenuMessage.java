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
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class OpenMenuMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "open_menu_message");

  private final UUID menuId;

  public OpenMenuMessage(final UUID uuid, final UUID menuId) {
    super(uuid);
    this.menuId = menuId;
  }

  public static OpenMenuMessage decode(final FriendlyByteBuf buffer) {
    return new OpenMenuMessage(buffer.readUUID(), buffer.readUUID());
  }

  public static FriendlyByteBuf encode(
      final OpenMenuMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getMenuId());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final OpenMenuMessage message, final ServerPlayer serverPlayer) {
    UUID uuid = message.getUUID();
    UUID menuId = message.getMenuId();
    log.info("Try open menu message for {} with menuId {} from {}", uuid, menuId, serverPlayer);
    MenuManager.openMenu(menuId);
  }

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public UUID getMenuId() {
    return this.menuId;
  }
}
