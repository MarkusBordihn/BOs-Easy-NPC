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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.attribute.CombatAttributes;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributes;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributes;
import de.markusbordihn.easynpc.data.attribute.MovementAttributeType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributes;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class AttributeHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private AttributeHandler() {}

  public static boolean setCombatAttribute(
      EasyNPC<?> easyNPC, CombatAttributeType attributeType, boolean value) {
    if (easyNPC == null || attributeType == null) {
      return false;
    }
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData == null || attributeData.getEntityAttributes() == null) {
      return false;
    }
    EntityAttributes entityAttributes = attributeData.getEntityAttributes();
    CombatAttributes attributes = entityAttributes.getCombatAttributes();
    log.debug("Changing combat attribute {}={} for {}", attributeType, value, easyNPC);
    switch (attributeType) {
      case IS_ATTACKABLE -> {
        entityAttributes.setCombatAttributes(attributes.withIsAttackable(value));
        if (easyNPC.getEntity() != null) {
          easyNPC.getEntity().setInvulnerable(!value);
        }
      }
      default -> {
        log.error("Unimplemented combat attribute {} for {}", attributeType, easyNPC);
        return false;
      }
    }
    attributeData.refreshEntityAttributes();
    return true;
  }

  public static boolean setCombatAttribute(
      EasyNPC<?> easyNPC, CombatAttributeType attributeType, double value) {
    if (easyNPC == null || attributeType == null) {
      return false;
    }
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData == null || attributeData.getEntityAttributes() == null) {
      return false;
    }
    EntityAttributes entityAttributes = attributeData.getEntityAttributes();
    CombatAttributes attributes = entityAttributes.getCombatAttributes();
    log.debug("Changing combat attribute {}={} for {}", attributeType, value, easyNPC);
    switch (attributeType) {
      case HEALTH_REGENERATION -> {
        entityAttributes.setCombatAttributes(attributes.withHealthRegeneration(value));
      }
      default -> {
        log.error("Unimplemented combat attribute {} for {}", attributeType, easyNPC);
        return false;
      }
    }
    attributeData.refreshEntityAttributes();
    return true;
  }

  public static boolean setEnvironmentalAttribute(
      EasyNPC<?> easyNPC, EnvironmentalAttributeType attributeType, boolean value) {
    if (easyNPC == null || attributeType == null) {
      return false;
    }
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData == null || attributeData.getEntityAttributes() == null) {
      return false;
    }
    EntityAttributes entityAttributes = attributeData.getEntityAttributes();
    EnvironmentalAttributes attributes = entityAttributes.getEnvironmentalAttributes();
    ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
    log.debug("Changing environmental attribute {}={} for {}", attributeType, value, easyNPC);
    switch (attributeType) {
      case CAN_BREATHE_UNDERWATER ->
          entityAttributes.setEnvironmentalAttributes(attributes.withCanBreathUnderwater(value));
      case CAN_FLOAT -> {
        entityAttributes.setEnvironmentalAttributes(attributes.withCanFloat(value));
        if (objectiveData != null) {
          objectiveData.registerAttributeBasedObjectives();
        }
        if (navigationData != null) {
          navigationData.refreshGroundNavigation();
        }
      }
      case FREEFALL -> entityAttributes.setEnvironmentalAttributes(attributes.withFreefall(value));
      default -> {
        log.error("Unimplemented environmental attribute {} for {}", attributeType, easyNPC);
        return false;
      }
    }
    attributeData.refreshEntityAttributes();
    return true;
  }

  public static boolean setInteractionAttribute(
      EasyNPC<?> easyNPC, InteractionAttributeType attributeType, boolean value) {
    if (easyNPC == null || attributeType == null) {
      return false;
    }
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData == null || attributeData.getEntityAttributes() == null) {
      return false;
    }
    EntityAttributes entityAttributes = attributeData.getEntityAttributes();
    InteractionAttributes attributes = entityAttributes.getInteractionAttributes();
    log.debug("Changing interaction attribute {}={} for {}", attributeType, value, easyNPC);
    switch (attributeType) {
      case CAN_BE_LEASHED ->
          entityAttributes.setInteractionAttributes(attributes.withCanBeLeashed(value));
      case IS_PUSHABLE ->
          entityAttributes.setInteractionAttributes(attributes.withIsPushable(value));
      case PUSH_ENTITIES ->
          entityAttributes.setInteractionAttributes(attributes.withPushEntities(value));
      default -> {
        log.error("Unimplemented interaction attribute {} for {}", attributeType, easyNPC);
        return false;
      }
    }
    attributeData.refreshEntityAttributes();
    return true;
  }

  public static boolean setMovementAttribute(
      EasyNPC<?> easyNPC, MovementAttributeType attributeType, boolean value) {
    if (easyNPC == null || attributeType == null) {
      return false;
    }
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData == null || attributeData.getEntityAttributes() == null) {
      return false;
    }
    EntityAttributes entityAttributes = attributeData.getEntityAttributes();
    MovementAttributes attributes = entityAttributes.getMovementAttributes();
    ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
    log.debug("Changing moving attribute {}={} for {}", attributeType, value, easyNPC);
    switch (attributeType) {
      case CAN_CLOSE_DOOR ->
          entityAttributes.setMovementAttributes(attributes.withCanCloseDoor(value));
      case CAN_OPEN_DOOR ->
          entityAttributes.setMovementAttributes(attributes.withCanOpenDoor(value));
      case CAN_PASS_DOOR ->
          entityAttributes.setMovementAttributes(attributes.withCanPassDoor(value));
      case CAN_USE_NETHER_PORTAL ->
          entityAttributes.setMovementAttributes(attributes.withCanUseNetherPortal(value));
      default -> {
        log.error("Unimplemented moving attribute {} for {}", attributeType, easyNPC);
        return false;
      }
    }

    // Refresh objectives and navigation data if available.
    attributeData.refreshEntityAttributes();
    if (objectiveData != null) {
      objectiveData.registerAttributeBasedObjectives();
    }
    if (navigationData != null) {
      navigationData.refreshGroundNavigation();
    }
    return true;
  }

  public static boolean setEntityAttribute(
      EasyNPC<?> easyNPC, EntityAttribute entityAttribute, boolean value) {
    if (easyNPC == null || entityAttribute == null) {
      return false;
    }
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData != null) {
      ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
      NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
      switch (entityAttribute) {
        case SILENT:
          log.debug("Change silent={} for {}", value, easyNPC);
          attributeData.setAttributeSilent(value);
          break;
        default:
          log.error("Unimplemented entity attribute {} for {}", entityAttribute, easyNPC);
          return false;
      }
      return true;
    }
    return false;
  }

  public static boolean setBaseAttribute(
      EasyNPC<?> easyNPC, ResourceLocation attribute, Double value) {
    if (easyNPC == null || attribute == null || value == null) {
      return false;
    }
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData != null) {
      switch (attribute.toString()) {
        case "minecraft:generic.max_health":
          attributeData.setBaseAttribute(Attributes.MAX_HEALTH, value);
          LivingEntity livingEntity = easyNPC.getLivingEntity();
          if (livingEntity != null) {
            livingEntity.setHealth(value.floatValue());
          }
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
          return false;
      }
      return true;
    }
    return false;
  }
}
