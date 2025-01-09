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
import de.markusbordihn.easynpc.data.display.DisplayAttributeEntry;
import de.markusbordihn.easynpc.data.display.DisplayAttributeSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeData;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ChangeDisplayAttributeMessage(
    UUID uuid,
    DisplayAttributeType displayAttributeType,
    Boolean booleanValue,
    Integer integerValue)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "change_display_attribute");
  public static final CustomPacketPayload.Type<ChangeDisplayAttributeMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeDisplayAttributeMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ChangeDisplayAttributeMessage::create);

  public ChangeDisplayAttributeMessage(
      final UUID uuid, final DisplayAttributeType displayAttributeType, final Boolean value) {
    this(uuid, displayAttributeType, value, 0);
  }

  public ChangeDisplayAttributeMessage(
      final UUID uuid, final DisplayAttributeType displayAttributeType, final Integer value) {
    this(uuid, displayAttributeType, false, value);
  }

  public static ChangeDisplayAttributeMessage create(final FriendlyByteBuf buffer) {
    return new ChangeDisplayAttributeMessage(
        buffer.readUUID(),
        buffer.readEnum(DisplayAttributeType.class),
        buffer.readBoolean(),
        buffer.readInt());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.displayAttributeType);
    buffer.writeBoolean(this.booleanValue);
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

    // Validate name.
    if (this.displayAttributeType == null) {
      log.error("Invalid entity attribute for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate value.
    if (this.booleanValue == null && this.integerValue == null) {
      log.error("Invalid value for {} for {} from {}", displayAttributeType, easyNPC, serverPlayer);
      return;
    }

    // Validate display attribute data.
    DisplayAttributeData<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    if (displayAttributeData == null) {
      log.error("Unable to get display attribute data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate display attribute set.
    DisplayAttributeSet displayAttributeSet = displayAttributeData.getDisplayAttributeSet();
    if (displayAttributeSet == null) {
      log.error("Unable to get display attribute set for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Update display attribute set.
    DisplayAttributeEntry displayAttributeEntry =
        new DisplayAttributeEntry(
            this.displayAttributeType, Boolean.TRUE.equals(this.booleanValue), this.integerValue);
    log.debug(
        "Change display attribute {} for {} to {}",
        this.displayAttributeType,
        easyNPC,
        displayAttributeEntry);
    displayAttributeSet.addOrReplaceDisplayAttribute(displayAttributeEntry);
    displayAttributeData.updateDisplayAttributeSet();

    // Force update of visibility, if visibility attribute has been changed.
    if (this.displayAttributeType == DisplayAttributeType.VISIBLE) {
      easyNPC.getEntity().setInvisible(Boolean.FALSE.equals(this.booleanValue));
    }
  }
}
