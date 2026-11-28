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

package de.markusbordihn.easynpc.configui.client.screen.configuration.attribute;

import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.attribute.BaseAttributeType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.player.Inventory;

public class BaseAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  private static final double MAX_SPEED_SLIDER_VALUE = 2.0D;
  private static final int SLIDER_TOP_SPACING = 20;
  private final Map<BaseAttributeType, RangeSliderButton> attributeSliders =
      new EnumMap<>(BaseAttributeType.class);

  public BaseAttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void addAttributeSlider(
      BaseAttributeType baseAttributeType, double value, double defaultValue, double stepSize) {
    this.addAttributeSlider(baseAttributeType, value, Double.MAX_VALUE, defaultValue, stepSize);
  }

  private void addAttributeSlider(
      BaseAttributeType baseAttributeType,
      double value,
      double maxSliderValue,
      double defaultValue,
      double stepSize) {
    RangeSliderButton slider =
        this.addRenderableWidget(
            new RangeSliderButton(
                this.buttonLeftPos + 130,
                this.buttonTopPos + 22 + this.attributeSliders.size() * SLIDER_TOP_SPACING,
                170,
                14,
                value,
                this.getBaseAttributes().getMinValue(baseAttributeType),
                Math.min(this.getBaseAttributes().getMaxValue(baseAttributeType), maxSliderValue),
                defaultValue,
                stepSize,
                changedSlider ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .entityBaseAttributeChange(
                            this.getEasyNPCUUID(),
                            baseAttributeType.getAttribute().value(),
                            changedSlider.getTargetDoubleValue())));
    this.attributeSliders.put(baseAttributeType, slider);
  }

  @Override
  public void init() {
    super.init();

    this.baseAttributeButton.active = false;
    this.attributeSliders.clear();

    LivingEntity livingEntity = this.getEasyNPC().getLivingEntity();

    this.addAttributeSlider(
        BaseAttributeType.MAX_HEALTH,
        livingEntity.getAttributeBaseValue(Attributes.MAX_HEALTH),
        20.0D,
        1.0D);
    this.addAttributeSlider(
        BaseAttributeType.FOLLOW_RANGE, this.getBaseAttributes().getFollowRange(), 32.0D, 1.0D);
    this.addAttributeSlider(
        BaseAttributeType.KNOCKBACK_RESISTANCE,
        this.getBaseAttributes().getKnockbackResistance(),
        0.0D,
        0.1D);

    if (livingEntity.getAttribute(Attributes.MOVEMENT_SPEED) != null) {
      this.addAttributeSlider(
          BaseAttributeType.MOVEMENT_SPEED,
          livingEntity.getAttributeBaseValue(Attributes.MOVEMENT_SPEED),
          MAX_SPEED_SLIDER_VALUE,
          0.6D,
          0.1D);
    }

    if (livingEntity.getAttribute(Attributes.FLYING_SPEED) != null
        && this.getEasyNPC().getEasyNPCNavigationData().canFly()) {
      this.addAttributeSlider(
          BaseAttributeType.FLYING_SPEED,
          livingEntity.getAttributeBaseValue(Attributes.FLYING_SPEED),
          MAX_SPEED_SLIDER_VALUE,
          0.4D,
          0.1D);
    }

    this.addAttributeSlider(
        BaseAttributeType.ATTACK_DAMAGE, this.getBaseAttributes().getAttackDamage(), 2.0D, 1.0D);
    this.addAttributeSlider(
        BaseAttributeType.ATTACK_KNOCKBACK,
        this.getBaseAttributes().getAttackKnockback(),
        0.0D,
        0.1D);

    if (livingEntity.getAttribute(Attributes.ATTACK_SPEED) != null) {
      this.addAttributeSlider(
          BaseAttributeType.ATTACK_SPEED,
          livingEntity.getAttributeBaseValue(Attributes.ATTACK_SPEED),
          4.0D,
          0.5D);
    }

    this.addAttributeSlider(
        BaseAttributeType.ARMOR, livingEntity.getAttributeBaseValue(Attributes.ARMOR), 0.0D, 1.0D);
    this.addAttributeSlider(
        BaseAttributeType.ARMOR_TOUGHNESS,
        livingEntity.getAttributeBaseValue(Attributes.ARMOR_TOUGHNESS),
        0.0D,
        0.5D);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    this.attributeSliders.forEach((baseAttributeType, slider) ->
        Text.drawConfigString(
            guiGraphics,
            this.font,
            baseAttributeType.getAttributeName(),
            slider.getX() - 135,
            slider.getY() + 3));
  }
}
