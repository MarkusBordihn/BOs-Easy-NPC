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
import de.markusbordihn.easynpc.client.screen.components.TextButton;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Locale;
import java.util.function.Consumer;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.AbstractWidget;
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
  private static final int WIDGET_WIDTH = 150;
  private static final int WIDGET_HEIGHT = 16;
  private static final int SAVE_BUTTON_WIDTH = 20;
  private static final int PAGE_BUTTON_WIDTH = 60;
  private static final int LABEL_TOP_OFFSET = 4;
  private static final int PROJECTILE_ITEM_MAXIMUM_LENGTH = 128;
  private static final int VALID_TEXT_COLOR = 0xFFE0E0E0;
  private static final int INVALID_TEXT_COLOR = 0xFFFF5555;
  private static final int ENABLED_TOP_OFFSET = 25;
  private static final int PAGE_BUTTON_TOP_OFFSET = 45;
  private static final int PROJECTILE_TYPE_TOP_OFFSET = 70;
  private static final int PROJECTILE_ITEM_TOP_OFFSET = 90;
  private static final int DAMAGE_TOP_OFFSET = 110;
  private static final int VELOCITY_TOP_OFFSET = 130;
  private static final int INACCURACY_TOP_OFFSET = 150;
  private static final int KNOCKBACK_TOP_OFFSET = 170;
  private static final int SHOT_COUNT_TOP_OFFSET = 190;
  private static final int TRAIL_PARTICLE_TOP_OFFSET = 70;
  private static final int IMPACT_EFFECT_TOP_OFFSET = 90;
  private static final int EXPLOSION_POWER_TOP_OFFSET = 110;
  private static final int HIT_EFFECT_TOP_OFFSET = 130;
  private static final int HIT_EFFECT_LEVEL_TOP_OFFSET = 150;
  private static final int HIT_EFFECT_SECONDS_TOP_OFFSET = 170;

  private final EnumMap<Page, List<AbstractWidget>> pageWidgets = new EnumMap<>(Page.class);
  private Page page = Page.PROJECTILE;
  private Button projectilePageButton;
  private Button effectsPageButton;
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

    this.pageWidgets.clear();
    for (Page widgetPage : Page.values()) {
      this.pageWidgets.put(widgetPage, new ArrayList<>());
    }

    this.projectileTypeSelectBox =
        this.addPageWidget(
            Page.PROJECTILE,
            this.createSelectBox(
                PROJECTILE_TYPE_TOP_OFFSET,
                createOptions(ProjectileType.values(), "projectile_type."),
                projectileType -> {
                  this.updateProjectileItemTextField(projectileType);
                  this.sendProjectileData();
                }));
    this.projectileTypeSelectBox.selectByValue(this.projectileData.projectileType());

    this.trailParticleSelectBox =
        this.addPageWidget(
            Page.EFFECTS,
            this.createSelectBox(
                TRAIL_PARTICLE_TOP_OFFSET,
                createOptions(ProjectileTrailParticle.values(), "projectile_trail_particle."),
                trailParticle -> this.sendProjectileData()));
    this.trailParticleSelectBox.selectByValue(this.projectileData.trailParticle());

    this.impactEffectSelectBox =
        this.addPageWidget(
            Page.EFFECTS,
            this.createSelectBox(
                IMPACT_EFFECT_TOP_OFFSET,
                createOptions(ProjectileImpactEffect.values(), "projectile_impact_effect."),
                impactEffect -> {
                  this.updateExplosionPowerSlider(impactEffect);
                  this.sendProjectileData();
                }));
    this.impactEffectSelectBox.selectByValue(this.projectileData.impactEffect());

    this.hitEffectSelectBox =
        this.addPageWidget(
            Page.EFFECTS,
            this.createSelectBox(
                HIT_EFFECT_TOP_OFFSET,
                createOptions(ProjectileHitEffect.values(), "projectile_hit_effect."),
                hitEffect -> {
                  this.updateHitEffectSliders(hitEffect);
                  this.sendProjectileData();
                }));
    this.hitEffectSelectBox.selectByValue(this.projectileData.hitEffect());

    this.projectilePageButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + LABEL_OFFSET,
                this.buttonTopPos + PAGE_BUTTON_TOP_OFFSET,
                PAGE_BUTTON_WIDTH,
                "projectile_page.projectile",
                onPress -> this.showPage(Page.PROJECTILE)));
    this.effectsPageButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + LABEL_OFFSET + PAGE_BUTTON_WIDTH + 2,
                this.buttonTopPos + PAGE_BUTTON_TOP_OFFSET,
                PAGE_BUTTON_WIDTH,
                "projectile_page.effects",
                onPress -> this.showPage(Page.EFFECTS)));

    this.enabledCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + LABEL_OFFSET,
                this.buttonTopPos + ENABLED_TOP_OFFSET,
                "projectile_enabled",
                this.projectileData.enabled(),
                checkbox -> this.sendProjectileData()));

    this.projectileItemTextField =
        this.addPageWidget(
            Page.PROJECTILE,
            new TextField(
                this.font,
                this.leftPos + WIDGET_OFFSET,
                this.buttonTopPos + PROJECTILE_ITEM_TOP_OFFSET,
                WIDGET_WIDTH - SAVE_BUTTON_WIDTH - 2,
                WIDGET_HEIGHT,
                this.projectileData.projectileItem().toString(),
                PROJECTILE_ITEM_MAXIMUM_LENGTH));
    this.projectileItemSaveButton =
        this.addPageWidget(
            Page.PROJECTILE,
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
        this.addPageWidget(
            Page.PROJECTILE,
            this.createSlider(
                DAMAGE_TOP_OFFSET,
                this.projectileData.damage(),
                ProjectileData.MINIMUM_DAMAGE,
                ProjectileData.MAXIMUM_DAMAGE,
                ProjectileData.DEFAULT_DAMAGE,
                0.5,
                SliderButton.Type.DOUBLE));

    this.velocitySlider =
        this.addPageWidget(
            Page.PROJECTILE,
            this.createSlider(
                VELOCITY_TOP_OFFSET,
                this.projectileData.velocity(),
                ProjectileData.MINIMUM_VELOCITY,
                ProjectileData.MAXIMUM_VELOCITY,
                ProjectileData.DEFAULT_VELOCITY,
                0.1,
                SliderButton.Type.DOUBLE));

    this.inaccuracySlider =
        this.addPageWidget(
            Page.PROJECTILE,
            this.createSlider(
                INACCURACY_TOP_OFFSET,
                this.projectileData.inaccuracy(),
                ProjectileData.MINIMUM_INACCURACY,
                ProjectileData.MAXIMUM_INACCURACY,
                ProjectileData.DEFAULT_INACCURACY,
                0.5,
                SliderButton.Type.DOUBLE));

    this.knockbackSlider =
        this.addPageWidget(
            Page.PROJECTILE,
            this.createSlider(
                KNOCKBACK_TOP_OFFSET,
                this.projectileData.knockback(),
                ProjectileData.MINIMUM_KNOCKBACK,
                ProjectileData.MAXIMUM_KNOCKBACK,
                ProjectileData.DEFAULT_KNOCKBACK,
                1,
                SliderButton.Type.INTEGER));

    this.shotCountSlider =
        this.addPageWidget(
            Page.PROJECTILE,
            this.createSlider(
                SHOT_COUNT_TOP_OFFSET,
                this.projectileData.shotCount(),
                ProjectileData.MINIMUM_SHOT_COUNT,
                ProjectileData.MAXIMUM_SHOT_COUNT,
                ProjectileData.DEFAULT_SHOT_COUNT,
                1,
                SliderButton.Type.INTEGER));

    this.explosionPowerSlider =
        this.addPageWidget(
            Page.EFFECTS,
            this.createSlider(
                EXPLOSION_POWER_TOP_OFFSET,
                Math.min(this.projectileData.explosionPower(), this.maximumExplosionPower),
                ProjectileData.MINIMUM_EXPLOSION_POWER,
                Math.max(this.maximumExplosionPower, 1),
                ProjectileData.DEFAULT_EXPLOSION_POWER,
                0.5,
                SliderButton.Type.DOUBLE));
    this.updateExplosionPowerSlider(this.projectileData.impactEffect());

    this.hitEffectLevelSlider =
        this.addPageWidget(
            Page.EFFECTS,
            this.createSlider(
                HIT_EFFECT_LEVEL_TOP_OFFSET,
                this.projectileData.hitEffectLevel(),
                ProjectileData.MINIMUM_HIT_EFFECT_LEVEL,
                ProjectileData.MAXIMUM_HIT_EFFECT_LEVEL,
                ProjectileData.DEFAULT_HIT_EFFECT_LEVEL,
                1,
                SliderButton.Type.INTEGER));

    this.hitEffectSecondsSlider =
        this.addPageWidget(
            Page.EFFECTS,
            this.createSlider(
                HIT_EFFECT_SECONDS_TOP_OFFSET,
                this.projectileData.hitEffectSeconds(),
                ProjectileData.MINIMUM_HIT_EFFECT_SECONDS,
                ProjectileData.MAXIMUM_HIT_EFFECT_SECONDS,
                ProjectileData.DEFAULT_HIT_EFFECT_SECONDS,
                1,
                SliderButton.Type.INTEGER));
    this.updateHitEffectSliders(this.projectileData.hitEffect());

    this.showPage(this.page);
  }

  private <W extends AbstractWidget> W addPageWidget(Page widgetPage, W widget) {
    this.pageWidgets.get(widgetPage).add(widget);
    return this.addRenderableWidget(widget);
  }

  private void showPage(Page shownPage) {
    this.page = shownPage;
    this.pageWidgets.forEach(
        (widgetPage, widgets) -> {
          for (AbstractWidget widget : widgets) {
            widget.visible = widgetPage == shownPage;
            if (!widget.visible) {
              widget.setFocused(false);
            }
          }
        });
    this.projectilePageButton.active = shownPage != Page.PROJECTILE;
    this.effectsPageButton.active = shownPage != Page.EFFECTS;
  }

  private <E> SelectBox<E> createSelectBox(
      int topOffset, List<SelectOption<E>> options, Consumer<E> onChange) {
    return new SelectBox<>(
        this.leftPos + WIDGET_OFFSET,
        this.buttonTopPos + topOffset,
        WIDGET_WIDTH,
        WIDGET_HEIGHT,
        options,
        onChange);
  }

  private RangeSliderButton createSlider(
      int topOffset,
      double value,
      double minimumValue,
      double maximumValue,
      double defaultValue,
      double stepSize,
      SliderButton.Type sliderType) {
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
        sliderType,
        true,
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

  private void drawLabel(GuiGraphicsExtractor guiGraphics, String translationKey, int topOffset) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        translationKey,
        this.leftPos + LABEL_OFFSET,
        this.buttonTopPos + topOffset + LABEL_TOP_OFFSET);
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);

    if (this.page == Page.PROJECTILE) {
      this.drawLabel(guiGraphics, "projectile_type", PROJECTILE_TYPE_TOP_OFFSET);
      this.drawLabel(guiGraphics, "projectile_item", PROJECTILE_ITEM_TOP_OFFSET);
      this.drawLabel(guiGraphics, "projectile_damage", DAMAGE_TOP_OFFSET);
      this.drawLabel(guiGraphics, "projectile_velocity", VELOCITY_TOP_OFFSET);
      this.drawLabel(guiGraphics, "projectile_inaccuracy", INACCURACY_TOP_OFFSET);
      this.drawLabel(guiGraphics, "projectile_knockback", KNOCKBACK_TOP_OFFSET);
      this.drawLabel(guiGraphics, "projectile_shot_count", SHOT_COUNT_TOP_OFFSET);
      return;
    }

    this.drawLabel(guiGraphics, "projectile_trail_particle", TRAIL_PARTICLE_TOP_OFFSET);
    this.drawLabel(guiGraphics, "projectile_impact_effect", IMPACT_EFFECT_TOP_OFFSET);
    this.drawLabel(guiGraphics, "projectile_explosion_power", EXPLOSION_POWER_TOP_OFFSET);
    this.drawLabel(guiGraphics, "projectile_hit_effect", HIT_EFFECT_TOP_OFFSET);
    this.drawLabel(guiGraphics, "projectile_hit_effect_level", HIT_EFFECT_LEVEL_TOP_OFFSET);
    this.drawLabel(guiGraphics, "projectile_hit_effect_seconds", HIT_EFFECT_SECONDS_TOP_OFFSET);
  }

  private enum Page {
    PROJECTILE,
    EFFECTS
  }
}
