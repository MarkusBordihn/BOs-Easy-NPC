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
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ChangeEntityAttributeMessage extends NetworkMessage<ChangeEntityAttributeMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_entity_attribute");

  protected final EntityAttribute entityAttribute;
  protected final Boolean booleanValue;
  protected final Float floatValue;
  protected final Integer integerValue;
  protected final String stringValue;

  public ChangeEntityAttributeMessage(
      final UUID uuid,
      final EntityAttribute entityAttribute,
      final Boolean booleanValue,
      final Float floatValue,
      final Integer integerValue,
      final String stringValue) {
    super(uuid);
    this.entityAttribute = entityAttribute;
    this.booleanValue = booleanValue;
    this.floatValue = floatValue;
    this.integerValue = integerValue;
    this.stringValue = stringValue;
  }

  public ChangeEntityAttributeMessage(
      final UUID uuid, final EntityAttribute entityAttribute, final Boolean value) {
    this(uuid, entityAttribute, value, 0f, 0, "");
  }

  public ChangeEntityAttributeMessage(
      final UUID uuid, final EntityAttribute entityAttribute, final Float value) {
    this(uuid, entityAttribute, false, value, 0, "");
  }

  public ChangeEntityAttributeMessage(
      final UUID uuid, final EntityAttribute entityAttribute, final Integer value) {
    this(uuid, entityAttribute, false, 0f, value, "");
  }

  public ChangeEntityAttributeMessage(
      final UUID uuid, final EntityAttribute entityAttribute, final String value) {
    this(uuid, entityAttribute, false, 0f, 0, value);
  }

  public static ChangeEntityAttributeMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeEntityAttributeMessage(
        buffer.readUUID(),
        buffer.readEnum(EntityAttribute.class),
        buffer.readBoolean(),
        buffer.readFloat(),
        buffer.readInt(),
        buffer.readUtf());
  }

  public static FriendlyByteBuf encode(
      final ChangeEntityAttributeMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getAttributeType());
    buffer.writeBoolean(message.getBooleanValue());
    buffer.writeFloat(message.getFloatValue());
    buffer.writeInt(message.getIntegerValue());
    buffer.writeUtf(message.getStringValue());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(
      final ChangeEntityAttributeMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate name.
    EntityAttribute entityAttribute = message.getAttributeType();
    if (entityAttribute == null) {
      log.error("Invalid entity attribute for {} from {}", message, serverPlayer);
      return;
    }

    // Validate value.
    Boolean booleanValue = message.getBooleanValue();
    Float floatValue = message.getFloatValue();
    Integer integerValue = message.getIntegerValue();
    String stringValue = message.getStringValue();
    if (booleanValue == null && floatValue == null && integerValue == null && stringValue == null) {
      log.error("Invalid value for {} for {} from {}", entityAttribute, message, serverPlayer);
      return;
    }

    EasyNPC<?> easyNPC = message.getEasyNPC();
    boolean successfullyChanged = false;
    if (entityAttribute == EntityAttribute.LIGHT_LEVEL) {
      successfullyChanged =
          AttributeHandler.setEntityAttribute(easyNPC, entityAttribute, integerValue);
    } else if (booleanValue != null) {
      successfullyChanged =
          AttributeHandler.setEntityAttribute(easyNPC, entityAttribute, booleanValue);
    }
    if (!successfullyChanged) {
      log.error(
          "Unable to change entity attribute {} for {} from {}",
          entityAttribute,
          easyNPC,
          serverPlayer);
    }
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public ChangeEntityAttributeMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public EntityAttribute getAttributeType() {
    return this.entityAttribute;
  }

  public Boolean getBooleanValue() {
    return this.booleanValue;
  }

  public Float getFloatValue() {
    return this.floatValue;
  }

  public Integer getIntegerValue() {
    return this.integerValue;
  }

  public String getStringValue() {
    return this.stringValue;
  }
}
