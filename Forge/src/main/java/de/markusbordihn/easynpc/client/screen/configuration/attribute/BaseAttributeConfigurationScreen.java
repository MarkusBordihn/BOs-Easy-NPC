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

import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.menu.configuration.attribute.BaseAttributeConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class BaseAttributeConfigurationScreen
    extends AttributeConfigurationScreen<BaseAttributeConfigurationMenu> {

  // Sliders
  SliderButton maxHealthSlider;
  SliderButton followRangeSlider;
  SliderButton knockbackResistanceSlider;
  SliderButton movementSpeedSlider;
  SliderButton flyingSpeedSlider;
  SliderButton attackDamageSlider;
  SliderButton attackKnockbackSlider;
  SliderButton attackSpeedSlider;
  SliderButton armorSlider;
  SliderButton armorToughnessSlider;

  public BaseAttributeConfigurationScreen(
      BaseAttributeConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  protected SliderButton createAttributeSlider(
      int left,
      int top,
      int width,
      int height,
      String name,
      double value,
      double minValue,
      double maxValue,
      double defaultValue,
      double stepSize,
      SliderButton.OnChange onChange) {
    SliderButton sliderButton =
        this.addRenderableWidget(
            new SliderButton(
                left + 12, top, width - 36, height, name, value, minValue, maxValue, onChange));
    this.addRenderableWidget(
        new TextButton(
            sliderButton.getX() - 12,
            top,
            12,
            height,
            Component.literal("-"),
            button -> {
              if (sliderButton.getTargetDoubleValue() - stepSize >= minValue) {
                sliderButton.setDefaultValue(sliderButton.getTargetDoubleValue() - stepSize);
                onChange.onChange(sliderButton);
              }
            }));
    Button plusButton =
        this.addRenderableWidget(
            new TextButton(
                sliderButton.getX() + sliderButton.getWidth(),
                top,
                12,
                height,
                Component.literal("+"),
                button -> {
                  if (sliderButton.getTargetDoubleValue() + stepSize <= maxValue) {
                    sliderButton.setDefaultValue(sliderButton.getTargetDoubleValue() + stepSize);
                    onChange.onChange(sliderButton);
                  }
                }));
    this.addRenderableWidget(
        new TextButton(
            plusButton.getX() + plusButton.getWidth(),
            top,
            12,
            height,
            Component.literal("↺"),
            button -> {
              sliderButton.setDefaultValue(defaultValue);
              onChange.onChange(sliderButton);
            }));
    return sliderButton;
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.baseAttributeButton.active = false;

    // Sliders
    int sliderXPos = this.buttonLeftPos + 125;
    int sliderYPos = this.buttonTopPos + 25;
    int sliderWidth = 170;
    int sliderHeight = 14;
    int sliderYSpace = 20;

    LivingEntity livingEntity = this.easyNPC.getLivingEntity();

    // Max Health
    this.maxHealthSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            "max_health",
            livingEntity.getAttributeBaseValue(Attributes.MAX_HEALTH),
            1.0D,
            1024.0D,
            20.0D,
            1.0D,
            slider ->
                NetworkMessageHandler.entityBaseAttributeChange(
                    uuid, Attributes.MAX_HEALTH, slider.getTargetDoubleValue()));

    // Follow Range
    sliderYPos += sliderYSpace;
    this.followRangeSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            "follow_range",
            this.menu.getFollowRange(),
            0.0D,
            2048.0D,
            32.0D,
            1.0D,
            slider ->
                NetworkMessageHandler.entityBaseAttributeChange(
                    uuid, Attributes.FOLLOW_RANGE, slider.getTargetDoubleValue()));

    // Knock-back Resistance
    sliderYPos += sliderYSpace;
    this.knockbackResistanceSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            "knockback_resistance",
            this.menu.getKnockbackResistance(),
            0.0D,
            1.0D,
            0.0D,
            0.1D,
            slider ->
                NetworkMessageHandler.entityBaseAttributeChange(
                    uuid, Attributes.KNOCKBACK_RESISTANCE, slider.getTargetDoubleValue()));

    // Movement Speed
    if (livingEntity.getAttribute(Attributes.MOVEMENT_SPEED) != null) {
      sliderYPos += sliderYSpace;
      this.movementSpeedSlider =
          createAttributeSlider(
              sliderXPos,
              sliderYPos,
              sliderWidth,
              sliderHeight,
              "movement_speed",
              livingEntity.getAttributeBaseValue(Attributes.MOVEMENT_SPEED),
              0.0D,
              2.0D,
              0.7D,
              0.1D,
              slider ->
                  NetworkMessageHandler.entityBaseAttributeChange(
                      uuid, Attributes.MOVEMENT_SPEED, slider.getTargetDoubleValue()));
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
              "flying_speed",
              livingEntity.getAttributeBaseValue(Attributes.FLYING_SPEED),
              0.0D,
              2.0D,
              0.4D,
              0.1D,
              slider ->
                  NetworkMessageHandler.entityBaseAttributeChange(
                      uuid, Attributes.FLYING_SPEED, slider.getTargetDoubleValue()));
    }

    // Attack Damage
    sliderYPos += sliderYSpace;
    this.attackDamageSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            "attack_damage",
            this.menu.getAttackDamage(),
            0.0D,
            2048.0D,
            2.0D,
            1.0D,
            slider ->
                NetworkMessageHandler.entityBaseAttributeChange(
                    uuid, Attributes.ATTACK_DAMAGE, slider.getTargetDoubleValue()));

    // Attack Knock-back
    sliderYPos += sliderYSpace;
    this.attackKnockbackSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            "attack_knockback",
            this.menu.getAttackKnockback(),
            0.0D,
            5.0D,
            0.0D,
            0.1D,
            slider ->
                NetworkMessageHandler.entityBaseAttributeChange(
                    uuid, Attributes.ATTACK_KNOCKBACK, slider.getTargetDoubleValue()));

    // Attack Speed
    if (livingEntity.getAttribute(Attributes.ATTACK_SPEED) != null) {
      sliderYPos += sliderYSpace;
      this.attackSpeedSlider =
          createAttributeSlider(
              sliderXPos,
              sliderYPos,
              sliderWidth,
              sliderHeight,
              "attack_speed",
              livingEntity.getAttributeBaseValue(Attributes.ATTACK_SPEED),
              0.0D,
              1024.0D,
              4.0D,
              0.5D,
              slider ->
                  NetworkMessageHandler.entityBaseAttributeChange(
                      uuid, Attributes.ATTACK_SPEED, slider.getTargetDoubleValue()));
    }

    // Armor
    sliderYPos += sliderYSpace;
    this.armorSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            "armor",
            livingEntity.getAttributeBaseValue(Attributes.ARMOR),
            0.0D,
            30.0D,
            0.0D,
            1.0D,
            slider ->
                NetworkMessageHandler.entityBaseAttributeChange(
                    uuid, Attributes.ARMOR, slider.getTargetDoubleValue()));

    // Armor Toughness
    sliderYPos += sliderYSpace;
    this.armorToughnessSlider =
        createAttributeSlider(
            sliderXPos,
            sliderYPos,
            sliderWidth,
            sliderHeight,
            "armor_toughness",
            livingEntity.getAttributeBaseValue(Attributes.ARMOR_TOUGHNESS),
            0.0D,
            20.0D,
            0.0D,
            0.5D,
            slider ->
                NetworkMessageHandler.entityBaseAttributeChange(
                    uuid, Attributes.ARMOR_TOUGHNESS, slider.getTargetDoubleValue()));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    int sliderXOffset = -145;
    int sliderYOffset = 3;

    if (this.maxHealthSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "max_health",
          this.maxHealthSlider.getX() + sliderXOffset,
          this.maxHealthSlider.getY() + sliderYOffset);
    }
    if (this.followRangeSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "follow_range",
          this.followRangeSlider.getX() + sliderXOffset,
          this.followRangeSlider.getY() + sliderYOffset);
    }
    if (this.knockbackResistanceSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "knockback_resistance",
          this.knockbackResistanceSlider.getX() + sliderXOffset,
          this.knockbackResistanceSlider.getY() + sliderYOffset);
    }
    if (this.movementSpeedSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "movement_speed",
          this.movementSpeedSlider.getX() + sliderXOffset,
          this.movementSpeedSlider.getY() + sliderYOffset);
    }
    if (this.flyingSpeedSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "flying_speed",
          this.flyingSpeedSlider.getX() + sliderXOffset,
          this.flyingSpeedSlider.getY() + sliderYOffset);
    }
    if (this.attackDamageSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "attack_damage",
          this.attackDamageSlider.getX() + sliderXOffset,
          this.attackDamageSlider.getY() + sliderYOffset);
    }
    if (this.attackKnockbackSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "attack_knockback",
          this.attackKnockbackSlider.getX() + sliderXOffset,
          this.attackKnockbackSlider.getY() + sliderYOffset);
    }
    if (this.attackSpeedSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "attack_speed",
          this.attackSpeedSlider.getX() + sliderXOffset,
          this.attackSpeedSlider.getY() + sliderYOffset);
    }
    if (this.armorSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "armor",
          this.armorSlider.getX() + sliderXOffset,
          this.armorSlider.getY() + sliderYOffset);
    }
    if (this.armorToughnessSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "armor_toughness",
          this.armorToughnessSlider.getX() + sliderXOffset,
          this.armorToughnessSlider.getY() + sliderYOffset);
    }
  }
}
