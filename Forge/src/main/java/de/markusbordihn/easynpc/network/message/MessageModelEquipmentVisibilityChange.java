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

package de.markusbordihn.easynpc.network.message;

import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraftforge.network.NetworkEvent;

public class MessageModelEquipmentVisibilityChange extends NetworkMessage {

  protected final EquipmentSlot equipmentSlot;
  protected final boolean visible;

  public MessageModelEquipmentVisibilityChange(
      UUID uuid, EquipmentSlot equipmentSlot, boolean visible) {
    super(uuid);
    this.equipmentSlot = equipmentSlot;
    this.visible = visible;
  }

  public static MessageModelEquipmentVisibilityChange decode(final FriendlyByteBuf buffer) {
    return new MessageModelEquipmentVisibilityChange(
        buffer.readUUID(), buffer.readEnum(EquipmentSlot.class), buffer.readBoolean());
  }

  public static void encode(
      final MessageModelEquipmentVisibilityChange message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getEquipmentSlot());
    buffer.writeBoolean(message.isVisible());
  }

  public static void handle(
      MessageModelEquipmentVisibilityChange message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageModelEquipmentVisibilityChange message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate ModelPart.
    EquipmentSlot equipmentSlot = message.getEquipmentSlot();
    if (equipmentSlot == null) {
      log.error("Invalid equipmentSlot for {} from {}", message, serverPlayer);
      return;
    }

    // Validate Model data.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      log.error("Invalid model data for {} from {}", message, serverPlayer);
      return;
    }

    // Validate Visibility.
    boolean visible = message.isVisible();

    // Perform action.
    log.debug(
        "Change {} visibility to {} for {} from {}", equipmentSlot, visible, easyNPC, serverPlayer);
    switch (equipmentSlot) {
      case HEAD:
        modelData.setModelHelmetVisible(visible);
        break;
      case CHEST:
        modelData.setModelChestplateVisible(visible);
        break;
      case LEGS:
        modelData.setModelLeggingsVisible(visible);
        break;
      case FEET:
        modelData.setModelBootsVisible(visible);
        break;
      default:
        log.error("Invalid equipmentSlot {} for {} from {}", equipmentSlot, message, serverPlayer);
        break;
    }
  }

  public EquipmentSlot getEquipmentSlot() {
    return this.equipmentSlot;
  }

  public boolean isVisible() {
    return this.visible;
  }
}
