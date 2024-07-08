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
import de.markusbordihn.easynpc.handler.AttributeHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ChangeEntityBaseAttributeMessage
    extends NetworkMessage<ChangeEntityBaseAttributeMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_entity_base_attribute");

  protected final ResourceLocation attribute;
  protected final Double value;

  public ChangeEntityBaseAttributeMessage(
      final UUID uuid, final ResourceLocation attribute, final Double value) {
    super(uuid);
    this.attribute = attribute;
    this.value = value;
  }

  public static ChangeEntityBaseAttributeMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeEntityBaseAttributeMessage(
        buffer.readUUID(), buffer.readResourceLocation(), buffer.readDouble());
  }

  public static FriendlyByteBuf encode(
      final ChangeEntityBaseAttributeMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeResourceLocation(message.attribute);
    buffer.writeDouble(message.value);
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(
      final ChangeEntityBaseAttributeMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate attribute.
    ResourceLocation attribute = message.getAttribute();
    if (attribute == null) {
      log.error("Invalid base attribute for {} from {}", message, serverPlayer);
      return;
    }

    // Validate value.
    Double value = message.getValue();
    if (value == null) {
      log.error(
          "Invalid value for base attribute {} for {} from {}", attribute, message, serverPlayer);
      return;
    }

    // Set base attribute.
    EasyNPC<?> easyNPC = message.getEasyNPC();
    if (!AttributeHandler.setBaseAttribute(easyNPC, attribute, value)) {
      log.error("Unable to set base attribute {} for {} from {}", attribute, message, serverPlayer);
    }
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public ChangeEntityBaseAttributeMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public ResourceLocation getAttribute() {
    return this.attribute;
  }

  public Double getValue() {
    return this.value;
  }
}
