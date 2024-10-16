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

package de.markusbordihn.easynpc.client.screen.configuration.attribute;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.SliderButton.OnChange;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attribute;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.attributes.RangedAttribute;
import net.minecraft.world.entity.player.Inventory;

public class BaseAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  RangeSliderButton armorSlider;
  RangeSliderButton armorToughnessSlider;
  RangeSliderButton attackDamageSlider;
  RangeSliderButton attackKnockbackSlider;
  RangeSliderButton attackSpeedSlider;
  RangeSliderButton flyingSpeedSlider;
  RangeSliderButton followRangeSlider;
  RangeSliderButton knockbackResistanceSlider;
  RangeSliderButton maxHealthSlider;
  RangeSliderButton movementSpeedSlider;

  public BaseAttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private RangeSliderButton createAttributeSlider(
      int left,
      int top,
      int width,
      int height,
      Attribute attribute,
      double value,
      double defaultMinValue,
      double defaultMaxValue,
      double defaultValue,
      double stepSize,
      OnChange onChange) {
    // Get attribute values directly from attribute with default value as fallback.
    double minValue =
        attribute instanceof RangedAttribute rangedAttribute
            ? rangedAttribute.getMinValue()
            : defaultMinValue;
    double maxValue =
        attribute instanceof RangedAttribute rangedAttribute
            ? rangedAttribute.getMaxValue()
            : defaultMaxValue;
    return this.addRenderableWidget(
        new RangeSliderButton(
            left,
            top,
            width,
            height,
            attribute.getDescriptionId(),
            value,
            minValue,
            maxValue,
            stepSize,
            defaultValue,
            onChange));
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.baseAttributeButton.active = false;

    // Sliders
    int sliderXPos = this.buttonLeftPos + 130;
    int sliderYPos = this.buttonTopPos + 22;
    int sliderWidth = 170;
    int sliderHeight = 14;
    int sliderYSpace = 20;

    // Attribute data
    LivingEntity livingEntity = this.getEasyNPC().getLivingEntity();

    // Max Health
    this.maxHealthSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            Attributes.MAX_HEALTH,
            livingEntity.getAttributeBaseValue(Attributes.MAX_HEALTH),
            1.0D,
            1024.0D,
            20.0D,
            1.0D,
            slider ->
                NetworkMessageHandlerManager.getServerHandler()
                    .entityBaseAttributeChange(
                        this.getEasyNPCUUID(),
                        Attributes.MAX_HEALTH,
                        slider.getTargetDoubleValue()));

    // Follow Range
    sliderYPos += sliderYSpace;
    this.followRangeSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            Attributes.FOLLOW_RANGE,
            this.getBaseAttributes().getFollowRange(),
            0.0D,
            2048.0D,
            32.0D,
            1.0D,
            slider ->
                NetworkMessageHandlerManager.getServerHandler()
                    .entityBaseAttributeChange(
                        this.getEasyNPCUUID(),
                        Attributes.FOLLOW_RANGE,
                        slider.getTargetDoubleValue()));

    // Knock-back Resistance
    sliderYPos += sliderYSpace;
    this.knockbackResistanceSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            Attributes.KNOCKBACK_RESISTANCE,
            this.getBaseAttributes().getKnockbackResistance(),
            0.0D,
            1.0D,
            0.0D,
            0.1D,
            slider ->
                NetworkMessageHandlerManager.getServerHandler()
                    .entityBaseAttributeChange(
                        this.getEasyNPCUUID(),
                        Attributes.KNOCKBACK_RESISTANCE,
                        slider.getTargetDoubleValue()));

    // Movement Speed
    if (livingEntity.getAttribute(Attributes.MOVEMENT_SPEED) != null) {
      sliderYPos += sliderYSpace;
      this.movementSpeedSlider =
          createAttributeSlider(
              sliderXPos,
              sliderYPos,
              sliderWidth,
              sliderHeight,
              Attributes.MOVEMENT_SPEED,
              livingEntity.getAttributeBaseValue(Attributes.MOVEMENT_SPEED),
              0.0D,
              2.0D,
              0.6D,
              0.1D,
              slider ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .entityBaseAttributeChange(
                          this.getEasyNPCUUID(),
                          Attributes.MOVEMENT_SPEED,
                          slider.getTargetDoubleValue()));
    }

    // Flying Speed
    if (livingEntity.getAttribute(Attributes.FLYING_SPEED) != null) {
      sliderYPos += sliderYSpace;
      this.flyingSpeedSlider =
          createAttributeSlider(
              sliderXPos,
              sliderYPos,
              sliderWidth,
              sliderHeight,
              Attributes.FLYING_SPEED,
              livingEntity.getAttributeBaseValue(Attributes.FLYING_SPEED),
              0.0D,
              2.0D,
              0.4D,
              0.1D,
              slider ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .entityBaseAttributeChange(
                          this.getEasyNPCUUID(),
                          Attributes.FLYING_SPEED,
                          slider.getTargetDoubleValue()));
    }

    // Attack Damage
    sliderYPos += sliderYSpace;
    this.attackDamageSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            Attributes.ATTACK_DAMAGE,
            this.getBaseAttributes().getAttackDamage(),
            0.0D,
            2048.0D,
            2.0D,
            1.0D,
            slider ->
                NetworkMessageHandlerManager.getServerHandler()
                    .entityBaseAttributeChange(
                        this.getEasyNPCUUID(),
                        Attributes.ATTACK_DAMAGE,
                        slider.getTargetDoubleValue()));

    // Attack Knock-back
    sliderYPos += sliderYSpace;
    this.attackKnockbackSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            Attributes.ATTACK_KNOCKBACK,
            this.getBaseAttributes().getAttackKnockback(),
            0.0D,
            5.0D,
            0.0D,
            0.1D,
            slider ->
                NetworkMessageHandlerManager.getServerHandler()
                    .entityBaseAttributeChange(
                        this.getEasyNPCUUID(),
                        Attributes.ATTACK_KNOCKBACK,
                        slider.getTargetDoubleValue()));

    // Attack Speed
    if (livingEntity.getAttribute(Attributes.ATTACK_SPEED) != null) {
      sliderYPos += sliderYSpace;
      this.attackSpeedSlider =
          createAttributeSlider(
              sliderXPos,
              sliderYPos,
              sliderWidth,
              sliderHeight,
              Attributes.ATTACK_SPEED,
              livingEntity.getAttributeBaseValue(Attributes.ATTACK_SPEED),
              0.0D,
              1024.0D,
              4.0D,
              0.5D,
              slider ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .entityBaseAttributeChange(
                          this.getEasyNPCUUID(),
                          Attributes.ATTACK_SPEED,
                          slider.getTargetDoubleValue()));
    }

    // Armor
    sliderYPos += sliderYSpace;
    this.armorSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            Attributes.ARMOR,
            livingEntity.getAttributeBaseValue(Attributes.ARMOR),
            0.0D,
            30.0D,
            0.0D,
            1.0D,
            slider ->
                NetworkMessageHandlerManager.getServerHandler()
                    .entityBaseAttributeChange(
                        this.getEasyNPCUUID(), Attributes.ARMOR, slider.getTargetDoubleValue()));

    // Armor Toughness
    sliderYPos += sliderYSpace;
    this.armorToughnessSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            Attributes.ARMOR_TOUGHNESS,
            livingEntity.getAttributeBaseValue(Attributes.ARMOR_TOUGHNESS),
            0.0D,
            20.0D,
            0.0D,
            0.5D,
            slider ->
                NetworkMessageHandlerManager.getServerHandler()
                    .entityBaseAttributeChange(
                        this.getEasyNPCUUID(),
                        Attributes.ARMOR_TOUGHNESS,
                        slider.getTargetDoubleValue()));
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    int sliderXOffset = -135;
    int sliderYOffset = 3;

    if (this.maxHealthSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "max_health",
          this.maxHealthSlider.x + sliderXOffset,
          this.maxHealthSlider.y + sliderYOffset);
    }
    if (this.followRangeSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "follow_range",
          this.followRangeSlider.x + sliderXOffset,
          this.followRangeSlider.y + sliderYOffset);
    }
    if (this.knockbackResistanceSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "knockback_resistance",
          this.knockbackResistanceSlider.x + sliderXOffset,
          this.knockbackResistanceSlider.y + sliderYOffset);
    }
    if (this.movementSpeedSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "movement_speed",
          this.movementSpeedSlider.x + sliderXOffset,
          this.movementSpeedSlider.y + sliderYOffset);
    }
    if (this.flyingSpeedSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "flying_speed",
          this.flyingSpeedSlider.x + sliderXOffset,
          this.flyingSpeedSlider.y + sliderYOffset);
    }
    if (this.attackDamageSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "attack_damage",
          this.attackDamageSlider.x + sliderXOffset,
          this.attackDamageSlider.y + sliderYOffset);
    }
    if (this.attackKnockbackSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "attack_knockback",
          this.attackKnockbackSlider.x + sliderXOffset,
          this.attackKnockbackSlider.y + sliderYOffset);
    }
    if (this.attackSpeedSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "attack_speed",
          this.attackSpeedSlider.x + sliderXOffset,
          this.attackSpeedSlider.y + sliderYOffset);
    }
    if (this.armorSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "armor",
          this.armorSlider.x + sliderXOffset,
          this.armorSlider.y + sliderYOffset);
    }
    if (this.armorToughnessSlider != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "armor_toughness",
          this.armorToughnessSlider.x + sliderXOffset,
          this.armorToughnessSlider.y + sliderYOffset);
    }
  }
}
