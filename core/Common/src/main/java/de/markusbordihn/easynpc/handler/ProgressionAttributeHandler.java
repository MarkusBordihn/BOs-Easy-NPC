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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ProgressionDataCapable;
import net.minecraft.core.Holder;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.ai.attributes.Attribute;
import net.minecraft.world.entity.ai.attributes.AttributeInstance;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ProgressionAttributeHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final ResourceLocation HEALTH_MODIFIER_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "progression_health_bonus");
  private static final ResourceLocation ATTACK_DAMAGE_MODIFIER_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "progression_attack_damage_bonus");

  private static final int BASE_HEALTH = 20;
  private static final int MAX_HEALTH = 100;
  private static final int BASE_ATTACK_DAMAGE = 1;
  private static final int MAX_ATTACK_DAMAGE = 20;

  private ProgressionAttributeHandler() {}

  public static void applyLevelScaling(EasyNPC<?> easyNPC) {
    if (easyNPC == null) return;

    ProgressionDataCapable<?> progressionData = easyNPC.getEasyNPCProgressionData();
    if (progressionData == null || !progressionData.isAttributeScalingEnabled()) {
      removeLevelScaling(easyNPC);
      return;
    }

    log.debug(
        "Applying level {} attribute scaling for {}",
        progressionData.getExperienceLevel(),
        easyNPC);

    applyAttributeModifier(
        easyNPC,
        progressionData,
        Attributes.MAX_HEALTH,
        HEALTH_MODIFIER_ID,
        "Progression Health Bonus",
        BASE_HEALTH,
        MAX_HEALTH);

    applyAttributeModifier(
        easyNPC,
        progressionData,
        Attributes.ATTACK_DAMAGE,
        ATTACK_DAMAGE_MODIFIER_ID,
        "Progression Attack Damage Bonus",
        BASE_ATTACK_DAMAGE,
        MAX_ATTACK_DAMAGE);

    float currentHealth = easyNPC.getLivingEntity().getHealth();
    float maxHealth = easyNPC.getLivingEntity().getMaxHealth();
    if (currentHealth > maxHealth) {
      easyNPC.getLivingEntity().setHealth(maxHealth);
    }
  }

  public static void removeLevelScaling(EasyNPC<?> easyNPC) {
    if (easyNPC == null) return;
    log.debug("Removing level-based attribute modifiers for {}", easyNPC);
    removeAttributeModifier(easyNPC, Attributes.MAX_HEALTH, HEALTH_MODIFIER_ID);
    removeAttributeModifier(easyNPC, Attributes.ATTACK_DAMAGE, ATTACK_DAMAGE_MODIFIER_ID);

    float currentHealth = easyNPC.getLivingEntity().getHealth();
    float maxHealth = easyNPC.getLivingEntity().getMaxHealth();
    if (currentHealth > maxHealth) {
      easyNPC.getLivingEntity().setHealth(maxHealth);
    }
  }

  private static void applyAttributeModifier(
      EasyNPC<?> easyNPC,
      ProgressionDataCapable<?> progressionData,
      Holder<Attribute> attribute,
      ResourceLocation modifierId,
      String name,
      int baseValue,
      int maxValue) {
    AttributeInstance attributeInstance = easyNPC.getLivingEntity().getAttribute(attribute);
    if (attributeInstance == null) return;

    int adjustment = progressionData.getAttributeAdjustment(baseValue, maxValue);
    if (adjustment <= 0) return;

    if (attributeInstance.getModifier(modifierId) != null) {
      attributeInstance.removeModifier(modifierId);
    }

    attributeInstance.addPermanentModifier(
        new AttributeModifier(modifierId, adjustment, AttributeModifier.Operation.ADD_VALUE));

    log.debug(
        "Applied {} modifier: +{} (level {})",
        name,
        adjustment,
        progressionData.getExperienceLevel());
  }

  private static void removeAttributeModifier(
      EasyNPC<?> easyNPC, Holder<Attribute> attribute, ResourceLocation modifierId) {
    AttributeInstance attributeInstance = easyNPC.getLivingEntity().getAttribute(attribute);
    if (attributeInstance != null && attributeInstance.getModifier(modifierId) != null) {
      attributeInstance.removeModifier(modifierId);
    }
  }
}
