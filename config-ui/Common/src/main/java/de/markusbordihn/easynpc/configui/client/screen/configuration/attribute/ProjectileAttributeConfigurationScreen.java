/*
 * Copyright 2026 Markus Bordihn
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
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectBox;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectOption;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.attack.ProjectileData;
import de.markusbordihn.easynpc.data.attack.ProjectileHitEffect;
import de.markusbordihn.easynpc.data.attack.ProjectileImpactEffect;
import de.markusbordihn.easynpc.data.attack.ProjectileTrailParticle;
import de.markusbordihn.easynpc.data.attack.ProjectileType;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.player.Inventory;

public class ProjectileAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  private static final int LABEL_OFFSET = 10;
  private static final int WIDGET_OFFSET = 110;
  private static final int WIDGET_WIDTH = 200;
  private static final int WIDGET_HEIGHT = 16;
  private static final int SELECT_BOX_WIDTH = 150;
  private static final int SECOND_COLUMN_LABEL_OFFSET = 168;
  private static final int FIRST_COLUMN_WIDGET_OFFSET = 72;
  private static final int SECOND_COLUMN_WIDGET_OFFSET = 225;
  private static final int COLUMN_WIDGET_WIDTH = 85;
  private static final int LABEL_TOP_OFFSET = 4;
  private static final int PROJECTILE_ITEM_MAXIMUM_LENGTH = 128;
  private static final int VALID_TEXT_COLOR = 0xE0E0E0;
  private static final int INVALID_TEXT_COLOR = 0xFF5555;
  private static final int ENABLED_TOP_OFFSET = 25;
  private static final int PROJECTILE_TYPE_TOP_OFFSET = 45;
  private static final int PROJECTILE_ITEM_TOP_OFFSET = 65;
  private static final int DAMAGE_TOP_OFFSET = 85;
  private static final int VELOCITY_TOP_OFFSET = 105;
  private static final int INACCURACY_TOP_OFFSET = 125;
  private static final int KNOCKBACK_AND_SHOT_COUNT_TOP_OFFSET = 145;
  private static final int TRAIL_AND_IMPACT_TOP_OFFSET = 165;
  private static final int HIT_EFFECT_AND_EXPLOSION_TOP_OFFSET = 185;
  private static final int HIT_EFFECT_LEVEL_AND_DURATION_TOP_OFFSET = 205;

  private ProjectileData projectileData = ProjectileData.DEFAULT;
  private int maximumExplosionPower = 0;
  private Checkbox enabledCheckbox;
  private SelectBox<ProjectileType> projectileTypeSelectBox;
  private TextField projectileItemTextField;
  private Button projectileItemSaveButton;
  private RangeSliderButton damageSlider;
  private RangeSliderButton velocitySlider;
  private RangeSliderButton inaccuracySlider;
  private RangeSliderButton knockbackSlider;
  private RangeSliderButton shotCountSlider;
  private SelectBox<ProjectileTrailParticle> trailParticleSelectBox;
  private SelectBox<ProjectileImpactEffect> impactEffectSelectBox;
  private SelectBox<ProjectileHitEffect> hitEffectSelectBox;
  private RangeSliderButton explosionPowerSlider;
  private RangeSliderButton hitEffectLevelSlider;
  private RangeSliderButton hitEffectSecondsSlider;

  public ProjectileAttributeConfigurationScreen(
      T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static <E extends Enum<E>> List<SelectOption<E>> createOptions(
      E[] values, String translationPrefix) {
    return Arrays.stream(values)
        .map(
            value ->
                SelectOption.of(
                    TextComponent.getTranslatedConfigText(
                            translationPrefix + value.name().toLowerCase(Locale.ROOT))
                        .getString(),
                    value))
        .toList();
  }

  private static Identifier parseProjectileItem(String value) {
    Identifier projectileItem = Identifier.tryParse(value);
    if (projectileItem == null || !BuiltInRegistries.ITEM.containsKey(projectileItem)) {
      return null;
    }

    return projectileItem;
  }

  @Override
  public void init() {
    super.init();

    AdditionalScreenData additionalScreenData = this.getAdditionalScreenData();
    if (additionalScreenData != null) {
      this.projectileData = additionalScreenData.getProjectileData();
      this.maximumExplosionPower = additionalScreenData.getProjectileMaximumExplosionPower();
    }

    int firstColumnLeftPos = this.leftPos + FIRST_COLUMN_WIDGET_OFFSET;
    int secondColumnLeftPos = this.leftPos + SECOND_COLUMN_WIDGET_OFFSET;

    this.projectileTypeSelectBox =
        this.addRenderableWidget(
            new SelectBox<>(
                this.leftPos + WIDGET_OFFSET,
                this.buttonTopPos + PROJECTILE_TYPE_TOP_OFFSET,
                SELECT_BOX_WIDTH,
                WIDGET_HEIGHT,
                createOptions(ProjectileType.values(), "projectile_type."),
                projectileType -> {
                  this.updateProjectileItemTextField(projectileType);
                  this.sendProjectileData();
                }));
    this.projectileTypeSelectBox.selectByValue(this.projectileData.projectileType());

    this.trailParticleSelectBox =
        this.addRenderableWidget(
            new SelectBox<>(
                firstColumnLeftPos,
                this.buttonTopPos + TRAIL_AND_IMPACT_TOP_OFFSET,
                COLUMN_WIDGET_WIDTH,
                WIDGET_HEIGHT,
                createOptions(ProjectileTrailParticle.values(), "projectile_trail_particle."),
                trailParticle -> this.sendProjectileData()));
    this.trailParticleSelectBox.selectByValue(this.projectileData.trailParticle());

    this.impactEffectSelectBox =
        this.addRenderableWidget(
            new SelectBox<>(
                secondColumnLeftPos,
                this.buttonTopPos + TRAIL_AND_IMPACT_TOP_OFFSET,
                COLUMN_WIDGET_WIDTH,
                WIDGET_HEIGHT,
                createOptions(ProjectileImpactEffect.values(), "projectile_impact_effect."),
                impactEffect -> {
                  this.updateExplosionPowerSlider(impactEffect);
                  this.sendProjectileData();
                }));
    this.impactEffectSelectBox.selectByValue(this.projectileData.impactEffect());

    this.hitEffectSelectBox =
        this.addRenderableWidget(
            new SelectBox<>(
                firstColumnLeftPos,
                this.buttonTopPos + HIT_EFFECT_AND_EXPLOSION_TOP_OFFSET,
                COLUMN_WIDGET_WIDTH,
                WIDGET_HEIGHT,
                createOptions(ProjectileHitEffect.values(), "projectile_hit_effect."),
                hitEffect -> {
                  this.updateHitEffectSliders(hitEffect);
                  this.sendProjectileData();
                }));
    this.hitEffectSelectBox.selectByValue(this.projectileData.hitEffect());

    this.enabledCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + LABEL_OFFSET,
                this.buttonTopPos + ENABLED_TOP_OFFSET,
                "projectile_enabled",
                this.projectileData.enabled(),
                checkbox -> this.sendProjectileData()));

    this.projectileItemTextField =
        this.addRenderableWidget(
            new TextField(
                this.font,
                this.leftPos + WIDGET_OFFSET,
                this.buttonTopPos + PROJECTILE_ITEM_TOP_OFFSET,
                SELECT_BOX_WIDTH,
                WIDGET_HEIGHT,
                this.projectileData.projectileItem().toString(),
                PROJECTILE_ITEM_MAXIMUM_LENGTH));
    this.projectileItemSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.projectileItemTextField.getX() + this.projectileItemTextField.getWidth() + 2,
                this.projectileItemTextField.getY() - 1,
                onPress -> this.saveProjectileItem()));
    this.projectileItemTextField.setResponder(
        projectileItem -> {
          this.projectileItemTextField.setTextColor(
              parseProjectileItem(projectileItem) != null
                  ? VALID_TEXT_COLOR
                  : INVALID_TEXT_COLOR);
          this.updateProjectileItemSaveButton();
        });
    this.updateProjectileItemTextField(this.projectileData.projectileType());

    this.damageSlider =
        this.addRenderableWidget(
            this.createSlider(
                DAMAGE_TOP_OFFSET,
                this.projectileData.damage(),
                ProjectileData.MINIMUM_DAMAGE,
                ProjectileData.MAXIMUM_DAMAGE,
                ProjectileData.DEFAULT_DAMAGE,
                0.5));

    this.velocitySlider =
        this.addRenderableWidget(
            this.createSlider(
                VELOCITY_TOP_OFFSET,
                this.projectileData.velocity(),
                ProjectileData.MINIMUM_VELOCITY,
                ProjectileData.MAXIMUM_VELOCITY,
                ProjectileData.DEFAULT_VELOCITY,
                0.1));

    this.inaccuracySlider =
        this.addRenderableWidget(
            this.createSlider(
                INACCURACY_TOP_OFFSET,
                this.projectileData.inaccuracy(),
                ProjectileData.MINIMUM_INACCURACY,
                ProjectileData.MAXIMUM_INACCURACY,
                ProjectileData.DEFAULT_INACCURACY,
                0.5));

    this.knockbackSlider =
        this.addRenderableWidget(
            this.createColumnSlider(
                firstColumnLeftPos,
                KNOCKBACK_AND_SHOT_COUNT_TOP_OFFSET,
                this.projectileData.knockback(),
                ProjectileData.MINIMUM_KNOCKBACK,
                ProjectileData.MAXIMUM_KNOCKBACK,
                ProjectileData.DEFAULT_KNOCKBACK,
                1,
                SliderButton.Type.INTEGER));

    this.shotCountSlider =
        this.addRenderableWidget(
            this.createColumnSlider(
                secondColumnLeftPos,
                KNOCKBACK_AND_SHOT_COUNT_TOP_OFFSET,
                this.projectileData.shotCount(),
                ProjectileData.MINIMUM_SHOT_COUNT,
                ProjectileData.MAXIMUM_SHOT_COUNT,
                ProjectileData.DEFAULT_SHOT_COUNT,
                1,
                SliderButton.Type.INTEGER));

    this.explosionPowerSlider =
        this.addRenderableWidget(
            this.createColumnSlider(
                secondColumnLeftPos,
                HIT_EFFECT_AND_EXPLOSION_TOP_OFFSET,
                Math.min(this.projectileData.explosionPower(), this.maximumExplosionPower),
                ProjectileData.MINIMUM_EXPLOSION_POWER,
                Math.max(this.maximumExplosionPower, 1),
                ProjectileData.DEFAULT_EXPLOSION_POWER,
                0.5,
                SliderButton.Type.DOUBLE));
    this.updateExplosionPowerSlider(this.projectileData.impactEffect());

    this.hitEffectLevelSlider =
        this.addRenderableWidget(
            this.createColumnSlider(
                firstColumnLeftPos,
                HIT_EFFECT_LEVEL_AND_DURATION_TOP_OFFSET,
                this.projectileData.hitEffectLevel(),
                ProjectileData.MINIMUM_HIT_EFFECT_LEVEL,
                ProjectileData.MAXIMUM_HIT_EFFECT_LEVEL,
                ProjectileData.DEFAULT_HIT_EFFECT_LEVEL,
                1,
                SliderButton.Type.INTEGER));

    this.hitEffectSecondsSlider =
        this.addRenderableWidget(
            this.createColumnSlider(
                secondColumnLeftPos,
                HIT_EFFECT_LEVEL_AND_DURATION_TOP_OFFSET,
                this.projectileData.hitEffectSeconds(),
                ProjectileData.MINIMUM_HIT_EFFECT_SECONDS,
                ProjectileData.MAXIMUM_HIT_EFFECT_SECONDS,
                ProjectileData.DEFAULT_HIT_EFFECT_SECONDS,
                1,
                SliderButton.Type.INTEGER));
    this.updateHitEffectSliders(this.projectileData.hitEffect());
  }

  private RangeSliderButton createSlider(
      int topOffset,
      double value,
      double minimumValue,
      double maximumValue,
      double defaultValue,
      double stepSize) {
    return new RangeSliderButton(
        this.leftPos + WIDGET_OFFSET,
        this.buttonTopPos + topOffset,
        WIDGET_WIDTH,
        WIDGET_HEIGHT,
        value,
        minimumValue,
        maximumValue,
        defaultValue,
        stepSize,
        SliderButton.Type.DOUBLE,
        true,
        slider -> {});
  }

  private RangeSliderButton createColumnSlider(
      int left,
      int topOffset,
      double value,
      double minimumValue,
      double maximumValue,
      double defaultValue,
      double stepSize,
      SliderButton.Type sliderType) {
    return new RangeSliderButton(
        left,
        this.buttonTopPos + topOffset,
        COLUMN_WIDGET_WIDTH,
        WIDGET_HEIGHT,
        value,
        minimumValue,
        maximumValue,
        defaultValue,
        stepSize,
        sliderType,
        false,
        slider -> {});
  }

  private void updateExplosionPowerSlider(ProjectileImpactEffect impactEffect) {
    this.explosionPowerSlider.active =
        this.maximumExplosionPower > 0 && impactEffect == ProjectileImpactEffect.EXPLOSION;
  }

  private void updateHitEffectSliders(ProjectileHitEffect hitEffect) {
    boolean hasHitEffect = hitEffect != ProjectileHitEffect.NONE;
    this.hitEffectLevelSlider.active = hasHitEffect;
    this.hitEffectSecondsSlider.active = hasHitEffect;
  }

  private void updateProjectileItemTextField(ProjectileType projectileType) {
    boolean isItemProjectile = projectileType == ProjectileType.ITEM;
    this.projectileItemTextField.setEditable(isItemProjectile);
    this.projectileItemTextField.active = isItemProjectile;
    this.updateProjectileItemSaveButton();
  }

  private Identifier getChangedProjectileItem() {
    Identifier projectileItem =
        parseProjectileItem(this.projectileItemTextField.getValue());
    if (projectileItem == null || projectileItem.equals(this.projectileData.projectileItem())) {
      return null;
    }

    return projectileItem;
  }

  private void updateProjectileItemSaveButton() {
    this.projectileItemSaveButton.active =
        this.projectileItemTextField.active && this.getChangedProjectileItem() != null;
  }

  private void saveProjectileItem() {
    Identifier projectileItem = this.getChangedProjectileItem();
    if (projectileItem == null) {
      return;
    }

    this.sendProjectileData(projectileItem);
    this.updateProjectileItemSaveButton();
  }

  private void sendProjectileData() {
    this.sendProjectileData(this.projectileData.projectileItem());
  }

  private void sendProjectileData(Identifier projectileItem) {
    ProjectileData updatedProjectileData =
        new ProjectileData(
            this.enabledCheckbox.selected(),
            this.damageSlider.getTargetValue(),
            this.velocitySlider.getTargetValue(),
            this.inaccuracySlider.getTargetValue(),
            this.trailParticleSelectBox.getSelectedValue(),
            this.impactEffectSelectBox.getSelectedValue(),
            Math.min(this.explosionPowerSlider.getTargetValue(), this.maximumExplosionPower),
            this.projectileTypeSelectBox.getSelectedValue(),
            projectileItem,
            Math.round(this.knockbackSlider.getTargetValue()),
            Math.round(this.shotCountSlider.getTargetValue()),
            this.hitEffectSelectBox.getSelectedValue(),
            Math.round(this.hitEffectLevelSlider.getTargetValue()),
            Math.round(this.hitEffectSecondsSlider.getTargetValue()));
    if (updatedProjectileData.equals(this.projectileData)) {
      return;
    }

    this.projectileData = updatedProjectileData;
    NetworkMessageHandlerManager.getServerHandler()
        .projectileDataChange(this.getEasyNPCUUID(), updatedProjectileData);
  }

  @Override
  public boolean mouseReleased(MouseButtonEvent mouseButtonEvent) {
    boolean result = super.mouseReleased(mouseButtonEvent);
    this.sendProjectileData();
    return result;
  }

  @Override
  public void removed() {
    this.sendProjectileData();
    super.removed();
  }

  private void drawLabel(GuiGraphics guiGraphics, String translationKey, int left, int topOffset) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        translationKey,
        this.leftPos + left,
        this.buttonTopPos + topOffset + LABEL_TOP_OFFSET);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    this.drawLabel(guiGraphics, "projectile_type", LABEL_OFFSET, PROJECTILE_TYPE_TOP_OFFSET);
    this.drawLabel(guiGraphics, "projectile_item", LABEL_OFFSET, PROJECTILE_ITEM_TOP_OFFSET);
    this.drawLabel(guiGraphics, "projectile_damage", LABEL_OFFSET, DAMAGE_TOP_OFFSET);
    this.drawLabel(guiGraphics, "projectile_velocity", LABEL_OFFSET, VELOCITY_TOP_OFFSET);
    this.drawLabel(guiGraphics, "projectile_inaccuracy", LABEL_OFFSET, INACCURACY_TOP_OFFSET);
    this.drawLabel(
        guiGraphics, "projectile_knockback", LABEL_OFFSET, KNOCKBACK_AND_SHOT_COUNT_TOP_OFFSET);
    this.drawLabel(
        guiGraphics,
        "projectile_shot_count",
        SECOND_COLUMN_LABEL_OFFSET,
        KNOCKBACK_AND_SHOT_COUNT_TOP_OFFSET);
    this.drawLabel(
        guiGraphics, "projectile_trail_particle", LABEL_OFFSET, TRAIL_AND_IMPACT_TOP_OFFSET);
    this.drawLabel(
        guiGraphics,
        "projectile_impact_effect",
        SECOND_COLUMN_LABEL_OFFSET,
        TRAIL_AND_IMPACT_TOP_OFFSET);
    this.drawLabel(
        guiGraphics, "projectile_hit_effect", LABEL_OFFSET, HIT_EFFECT_AND_EXPLOSION_TOP_OFFSET);
    this.drawLabel(
        guiGraphics,
        "projectile_explosion_power",
        SECOND_COLUMN_LABEL_OFFSET,
        HIT_EFFECT_AND_EXPLOSION_TOP_OFFSET);
    this.drawLabel(
        guiGraphics,
        "projectile_hit_effect_level",
        LABEL_OFFSET,
        HIT_EFFECT_LEVEL_AND_DURATION_TOP_OFFSET);
    this.drawLabel(
        guiGraphics,
        "projectile_hit_effect_seconds",
        SECOND_COLUMN_LABEL_OFFSET,
        HIT_EFFECT_LEVEL_AND_DURATION_TOP_OFFSET);
  }
}
