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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.Style;
import net.minecraft.network.chat.TextColor;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ChangeNameMessage extends NetworkMessage<ChangeNameMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_name");

  private final String name;
  private final int color;

  public ChangeNameMessage(final UUID uuid, final String name, final int color) {
    super(uuid);
    this.name = name;
    this.color = color;
  }

  public static ChangeNameMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeNameMessage(buffer.readUUID(), buffer.readUtf(), buffer.readInt());
  }

  public static FriendlyByteBuf encode(
      final ChangeNameMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUtf(message.getName());
    buffer.writeInt(message.getColor());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final ChangeNameMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate name.
    String name = message.getName();
    if (name == null || name.isEmpty()) {
      log.error("Invalid name {} for {} from {}", name, message, serverPlayer);
      return;
    }

    // Validate color.
    int color = message.getColor();

    // Perform action.
    EasyNPC<?> easyNPC = message.getEasyNPC();
    if (color >= 0) {
      Style style = Style.EMPTY.withColor(TextColor.fromRgb(color));
      easyNPC.getEntity().setCustomName(Component.literal(name).setStyle(style));
    } else {
      easyNPC.getEntity().setCustomName(Component.literal(name));
    }
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public ChangeNameMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public String getName() {
    return this.name;
  }

  public int getColor() {
    return this.color;
  }
}
