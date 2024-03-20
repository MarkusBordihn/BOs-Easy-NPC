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
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraftforge.network.NetworkEvent;

public class MessageEntityBaseAttributeChange extends NetworkMessage {

  protected final ResourceLocation attribute;
  protected final Double value;

  public MessageEntityBaseAttributeChange(UUID uuid, ResourceLocation attribute, Double value) {
    super(uuid);
    this.attribute = attribute;
    this.value = value;
  }

  public static MessageEntityBaseAttributeChange decode(final FriendlyByteBuf buffer) {
    return new MessageEntityBaseAttributeChange(
        buffer.readUUID(), buffer.readResourceLocation(), buffer.readDouble());
  }

  public static void encode(
      final MessageEntityBaseAttributeChange message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeResourceLocation(message.attribute);
    buffer.writeDouble(message.value);
  }

  public static void handle(
      MessageEntityBaseAttributeChange message, Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageEntityBaseAttributeChange message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
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

    // Validate entity.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to find entity {} for {} from {}", uuid, message, serverPlayer);
      return;
    }

    // Validate attribute data.
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData == null) {
      log.error("Attribute data for {} is not available for {}", easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    String attributeString = attribute.toString();
    switch (attributeString) {
      case "minecraft:generic.max_health":
        attributeData.setBaseAttribute(Attributes.MAX_HEALTH, value);
        easyNPC.getLivingEntity().setHealth(value.floatValue());
        break;
      case "minecraft:generic.follow_range":
        attributeData.setBaseAttribute(Attributes.FOLLOW_RANGE, value);
        break;
      case "minecraft:generic.knockback_resistance":
        attributeData.setBaseAttribute(Attributes.KNOCKBACK_RESISTANCE, value);
        break;
      case "minecraft:generic.movement_speed":
        attributeData.setBaseAttribute(Attributes.MOVEMENT_SPEED, value);
        break;
      case "minecraft:generic.flying_speed":
        attributeData.setBaseAttribute(Attributes.FLYING_SPEED, value);
        break;
      case "minecraft:generic.attack_damage":
        attributeData.setBaseAttribute(Attributes.ATTACK_DAMAGE, value);
        break;
      case "minecraft:generic.attack_knockback":
        attributeData.setBaseAttribute(Attributes.ATTACK_KNOCKBACK, value);
        break;
      case "minecraft:generic.attack_speed":
        attributeData.setBaseAttribute(Attributes.ATTACK_SPEED, value);
        break;
      case "minecraft:generic.armor":
        attributeData.setBaseAttribute(Attributes.ARMOR, value);
        break;
      case "minecraft:generic.armor_toughness":
        attributeData.setBaseAttribute(Attributes.ARMOR_TOUGHNESS, value);
        break;
      case "minecraft:generic.luck":
        attributeData.setBaseAttribute(Attributes.LUCK, value);
        break;
      default:
        log.error("Unknown attribute {} for {} from {}", attribute, message, serverPlayer);
    }
  }

  public ResourceLocation getAttribute() {
    return this.attribute;
  }

  public Double getValue() {
    return this.value;
  }
}
