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
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ChangeEnvironmentalAttributeMessage(
    UUID uuid,
    EnvironmentalAttributeType attributeType,
    Boolean booleanValue,
    Double doubleValue,
    Integer integerValue)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_environmental_attribute");

  public ChangeEnvironmentalAttributeMessage(
      final UUID uuid, final EnvironmentalAttributeType attributeType, final Boolean value) {
    this(uuid, attributeType, value, 0d, 0);
  }

  public static ChangeEnvironmentalAttributeMessage create(final FriendlyByteBuf buffer) {
    return new ChangeEnvironmentalAttributeMessage(
        buffer.readUUID(),
        buffer.readEnum(EnvironmentalAttributeType.class),
        buffer.readBoolean(),
        buffer.readDouble(),
        buffer.readInt());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.attributeType);
    buffer.writeBoolean(this.booleanValue);
    buffer.writeDouble(this.doubleValue);
    buffer.writeInt(this.integerValue);
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    // Validate value.
    if (booleanValue == null && doubleValue == null && integerValue == null) {
      log.error("Invalid value for {} for {} from {}", attributeType, easyNPC, serverPlayer);
      return;
    }

    // Update attribute value.
    if (booleanValue != null) {
      AttributeHandler.setEnvironmentalAttribute(easyNPC, attributeType, booleanValue);
    }
  }
}
