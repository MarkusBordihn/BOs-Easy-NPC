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

import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class MessageEntityAttributeChange extends NetworkMessage {

  protected final EntityAttribute entityAttribute;
  protected final Boolean booleanValue;
  protected final Float floatValue;
  protected final Integer integerValue;
  protected final String stringValue;

  public MessageEntityAttributeChange(
      UUID uuid,
      EntityAttribute entityAttribute,
      Boolean booleanValue,
      Float floatValue,
      Integer integerValue,
      String stringValue) {
    super(uuid);
    this.entityAttribute = entityAttribute;
    this.booleanValue = booleanValue;
    this.floatValue = floatValue;
    this.integerValue = integerValue;
    this.stringValue = stringValue;
  }

  public MessageEntityAttributeChange(UUID uuid, EntityAttribute entityAttribute, Boolean value) {
    this(uuid, entityAttribute, value, 0f, 0, "");
  }

  public MessageEntityAttributeChange(UUID uuid, EntityAttribute entityAttribute, Float value) {
    this(uuid, entityAttribute, false, value, 0, "");
  }

  public MessageEntityAttributeChange(UUID uuid, EntityAttribute entityAttribute, Integer value) {
    this(uuid, entityAttribute, false, 0f, value, "");
  }

  public MessageEntityAttributeChange(UUID uuid, EntityAttribute entityAttribute, String value) {
    this(uuid, entityAttribute, false, 0f, 0, value);
  }

  public static MessageEntityAttributeChange decode(final FriendlyByteBuf buffer) {
    return new MessageEntityAttributeChange(
        buffer.readUUID(),
        buffer.readEnum(EntityAttribute.class),
        buffer.readBoolean(),
        buffer.readFloat(),
        buffer.readInt(),
        buffer.readUtf());
  }

  public static void encode(
      final MessageEntityAttributeChange message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getAttributeType());
    buffer.writeBoolean(message.getBooleanValue());
    buffer.writeFloat(message.getFloatValue());
    buffer.writeInt(message.getIntegerValue());
    buffer.writeUtf(message.getStringValue());
  }

  public static void handle(
      MessageEntityAttributeChange message, Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageEntityAttributeChange message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate name.
    EntityAttribute entityAttribute = message.getAttributeType();
    if (entityAttribute == null) {
      log.error(
          "Invalid entity attribute {} for {} from {}", entityAttribute, message, serverPlayer);
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

    // Validate entity.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to find entity {} for {} from {}", uuid, message, serverPlayer);
      return;
    }

    // Validate attribute data
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData == null) {
      log.error("Unable to find attribute data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate objectives.
    ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null) {
      log.error("Unable to find objective data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Update entity attribute.
    switch (entityAttribute) {
      case FREEFALL:
        if (booleanValue != null) {
          log.debug("Change freefall={} for {} from {}", booleanValue, easyNPC, serverPlayer);
          attributeData.setAttributeFreefall(booleanValue);
        }
        break;
      case CAN_FLOAT:
        if (booleanValue != null) {
          log.debug("Change canFloat={} for {} from {}", booleanValue, easyNPC, serverPlayer);
          attributeData.setAttributeCanFloat(booleanValue);
          objectiveData.registerAttributeBasedObjectives();
        }
        break;
      case CAN_OPEN_DOOR:
        if (booleanValue != null) {
          log.debug("Change canOpenDoor={} for {} from {}", booleanValue, easyNPC, serverPlayer);
          attributeData.setAttributeCanOpenDoor(booleanValue);
          objectiveData.registerAttributeBasedObjectives();
        }
        break;
      case CAN_CLOSE_DOOR:
        if (booleanValue != null) {
          log.debug("Change canCloseDoor={} for {} from {}", booleanValue, easyNPC, serverPlayer);
          attributeData.setAttributeCanCloseDoor(booleanValue);
          objectiveData.registerAttributeBasedObjectives();
        }
        break;
      case CAN_PASS_DOOR:
        if (booleanValue != null) {
          log.debug("Change canPassDoor={} for {} from {}", booleanValue, easyNPC, serverPlayer);
          attributeData.setAttributeCanPassDoor(booleanValue);
          objectiveData.registerAttributeBasedObjectives();
        }
        break;
      case CAN_USE_NETHER_PORTAL:
        if (booleanValue != null) {
          log.debug(
              "Change canUseNetherPortal={} for {} from {}", booleanValue, easyNPC, serverPlayer);
          attributeData.setAttributeCanUseNetherPortal(booleanValue);
        }
        break;
      case IS_ATTACKABLE:
        if (booleanValue != null) {
          log.debug("Change isAttackable={} for {} from {}", booleanValue, easyNPC, serverPlayer);
          attributeData.setAttributeIsAttackable(booleanValue);
          if (easyNPC.getLivingEntity() != null) {
            easyNPC.getLivingEntity().setInvulnerable(!booleanValue);
          }
        }
        break;
      case IS_PUSHABLE:
        if (booleanValue != null) {
          log.debug("Change isPushable={} for {} from {}", booleanValue, easyNPC, serverPlayer);
          attributeData.setAttributeIsPushable(booleanValue);
        }
        break;
      case LIGHT_LEVEL:
        if (integerValue != null) {
          log.debug("Change lightLevel={} for {} from {}", integerValue, easyNPC, serverPlayer);
          attributeData.setAttributeLightLevel(integerValue);
        }
        break;
      default:
        log.error(
            "Unimplemented entity attribute {} for {} from {}",
            entityAttribute,
            message,
            serverPlayer);
        break;
    }
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
