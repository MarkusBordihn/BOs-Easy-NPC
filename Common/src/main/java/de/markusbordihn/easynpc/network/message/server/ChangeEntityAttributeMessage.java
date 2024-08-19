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
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ChangeEntityAttributeMessage(
    UUID uuid,
    EntityAttribute entityAttribute,
    Boolean booleanValue,
    Float floatValue,
    Integer integerValue,
    String stringValue)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_entity_attribute");
  public static final CustomPacketPayload.Type<ChangeEntityAttributeMessage> PAYLOAD_TYPE =
      CustomPacketPayload.createType(MESSAGE_ID.toString());
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeEntityAttributeMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ChangeEntityAttributeMessage::create);

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

  public static ChangeEntityAttributeMessage create(final FriendlyByteBuf buffer) {
    return new ChangeEntityAttributeMessage(
        buffer.readUUID(),
        buffer.readEnum(EntityAttribute.class),
        buffer.readBoolean(),
        buffer.readFloat(),
        buffer.readInt(),
        buffer.readUtf());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.entityAttribute);
    buffer.writeBoolean(this.booleanValue);
    buffer.writeFloat(this.floatValue);
    buffer.writeInt(this.integerValue);
    buffer.writeUtf(this.stringValue);
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

    // Validate name.
    if (entityAttribute == null) {
      log.error("Invalid entity attribute for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate value.
    if (booleanValue == null && floatValue == null && integerValue == null && stringValue == null) {
      log.error("Invalid value for {} for {} from {}", entityAttribute, easyNPC, serverPlayer);
      return;
    }

    boolean successfullyChanged = false;
    if (booleanValue != null) {
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
}
