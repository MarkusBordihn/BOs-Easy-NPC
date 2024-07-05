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
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attribute;
import net.minecraft.world.entity.ai.attributes.Attributes;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class AttributeHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private AttributeHandler() {}

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
        case FREEFALL:
          log.debug("Change freefall={} for {}", value, easyNPC);
          attributeData.setAttributeFreefall(value);
          break;
        case CAN_BE_LEASHED:
          log.debug("Change canBeLeashed={} for {}", value, easyNPC);
          attributeData.setAttributeCanBeLeashed(value);
          break;
        case CAN_FLOAT:
          log.debug("Change canFloat={} for {}", value, easyNPC);
          attributeData.setAttributeCanFloat(value);
          if (objectiveData != null) {
            objectiveData.registerAttributeBasedObjectives();
          }
          if (navigationData != null) {
            navigationData.refreshGroundNavigation();
          }
          break;
        case CAN_OPEN_DOOR:
          log.debug("Change canOpenDoor={} for {}", value, easyNPC);
          attributeData.setAttributeCanOpenDoor(value);
          if (navigationData != null) {
            navigationData.refreshGroundNavigation();
          }
          if (objectiveData != null) {
            objectiveData.registerAttributeBasedObjectives();
          }
          break;
        case CAN_CLOSE_DOOR:
          log.debug("Change canCloseDoor={} for {}", value, easyNPC);
          attributeData.setAttributeCanCloseDoor(value);
          if (objectiveData != null) {
            objectiveData.registerAttributeBasedObjectives();
          }
          break;
        case CAN_PASS_DOOR:
          log.debug("Change canPassDoor={} for {}", value, easyNPC);
          attributeData.setAttributeCanPassDoor(value);
          if (navigationData != null) {
            navigationData.refreshGroundNavigation();
          }
          break;
        case CAN_USE_NETHER_PORTAL:
          log.debug("Change canUseNetherPortal={} for {}", value, easyNPC);
          attributeData.setAttributeCanUseNetherPortal(value);
          break;
        case IS_ATTACKABLE:
          log.debug("Change isAttackable={} for {}", value, easyNPC);
          attributeData.setAttributeIsAttackable(value);
          if (easyNPC.getLivingEntity() != null) {
            easyNPC.getLivingEntity().setInvulnerable(!value);
          }
          break;
        case IS_PUSHABLE:
          log.debug("Change isPushable={} for {}", value, easyNPC);
          attributeData.setAttributeIsPushable(value);
          break;
        case PUSH_ENTITIES:
          log.debug("Change pushEntities={} for {}", value, easyNPC);
          attributeData.setAttributePushEntities(value);
          break;
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

  public static boolean setEntityAttribute(
      EasyNPC<?> easyNPC, EntityAttribute entityAttribute, Integer value) {
    if (easyNPC == null || entityAttribute == null) {
      return false;
    }
    AttributeData<?> attributeData = easyNPC.getEasyNPCAttributeData();
    if (attributeData != null) {
      switch (entityAttribute) {
        case LIGHT_LEVEL:
          log.debug("Change lightLevel={} for {}", value, easyNPC);
          attributeData.setAttributeLightLevel(value);
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

  public static Attribute getAttribute(ResourceLocation resourceLocation) {
    if (resourceLocation == null) {
      return null;
    }
    return Registry.ATTRIBUTE.getOptional(resourceLocation).orElse(null);
  }
}
