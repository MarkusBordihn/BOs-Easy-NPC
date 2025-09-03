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

package de.markusbordihn.easynpc.configui.network.message.server;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.type.ValueType;
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

public record ChangeCombatAttributeMessage(
    UUID uuid,
    CombatAttributeType attributeType,
    ValueType valueType,
    boolean booleanValue,
    double doubleValue,
    int integerValue)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "change_combat_attribute");
  public static final Type<ChangeCombatAttributeMessage> PAYLOAD_TYPE = new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeCombatAttributeMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ChangeCombatAttributeMessage::create);

  public ChangeCombatAttributeMessage(
      final UUID uuid, final CombatAttributeType attributeType, final Boolean value) {
    this(uuid, attributeType, ValueType.BOOLEAN, value, 0.0d, 0);
  }

  public ChangeCombatAttributeMessage(
      final UUID uuid, final CombatAttributeType attributeType, final Double value) {
    this(uuid, attributeType, ValueType.DOUBLE, false, value, 0);
  }

  public static ChangeCombatAttributeMessage create(final FriendlyByteBuf buffer) {
    return new ChangeCombatAttributeMessage(
        buffer.readUUID(),
        buffer.readEnum(CombatAttributeType.class),
        buffer.readEnum(ValueType.class),
        buffer.readBoolean(),
        buffer.readDouble(),
        buffer.readInt());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.attributeType);
    buffer.writeEnum(this.valueType);
    buffer.writeBoolean(this.booleanValue);
    buffer.writeDouble(this.doubleValue);
    buffer.writeInt(this.integerValue);
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

    // Update attribute value.
    switch (valueType) {
      case BOOLEAN -> AttributeHandler.setCombatAttribute(easyNPC, attributeType, booleanValue);
      case DOUBLE -> AttributeHandler.setCombatAttribute(easyNPC, attributeType, doubleValue);
      default ->
          log.error(
              "Invalid combat value type for {} for {} from {}",
              attributeType,
              easyNPC,
              serverPlayer);
    }
  }
}
