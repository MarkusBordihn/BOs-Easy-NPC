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

package de.markusbordihn.easynpc.configui.client.screen.configuration.objective;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.ItemToggleButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;

public class AttackObjectiveConfigurationScreen<T extends ConfigurationMenu>
    extends ObjectiveConfigurationScreen<T> {

  private static final List<ObjectiveType> ATTACK_STYLES =
      List.of(
          ObjectiveType.MELEE_ATTACK,
          ObjectiveType.ZOMBIE_ATTACK,
          ObjectiveType.BOW_ATTACK,
          ObjectiveType.CROSSBOW_ATTACK,
          ObjectiveType.GUN_ATTACK,
          ObjectiveType.PROJECTILE_ATTACK);
  private static final double DEFAULT_SPEED_MODIFIER = 0.6D;
  private static final int CONTENT_WIDTH = 300;
  private static final int ROW_HEIGHT = 18;
  private static final int SECTION_LINE_COLOR = 0x60808080;
  private static final int SUMMARY_COLOR = 0xFF404040;
  private static final int WARNING_COLOR = 0xFFAA0000;

  private final Map<ObjectiveType, ItemToggleButton> attackStyleButtons =
      new EnumMap<>(ObjectiveType.class);
  protected Checkbox attackPlayerCheckbox;
  protected Checkbox spareOwnerCheckbox;
  protected Checkbox attackVillagerCheckbox;
  protected Checkbox attackAnimalCheckbox;
  protected Checkbox attackMonsterCheckbox;
  protected Checkbox attackMobCheckbox;
  protected Checkbox spareCreeperCheckbox;
  protected Checkbox hurtByTargetCheckbox;
  protected Checkbox ownerHurtByTargetCheckbox;
  protected Checkbox factionHurtByTargetCheckbox;
  protected Checkbox ownerHurtTargetCheckbox;
  private int howSectionTop;
  private int whoSectionTop;
  private int whenSectionTop;
  private List<FormattedCharSequence> attackSummaryLines = Collections.emptyList();
  private List<FormattedCharSequence> attackWarningLines = Collections.emptyList();

  public AttackObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static Item getAttackStyleIcon(ObjectiveType attackStyle) {
    return switch (attackStyle) {
      case MELEE_ATTACK -> Items.IRON_SWORD;
      case ZOMBIE_ATTACK -> Items.ZOMBIE_HEAD;
      case BOW_ATTACK -> Items.BOW;
      case CROSSBOW_ATTACK -> Items.CROSSBOW;
      case GUN_ATTACK -> Items.SPYGLASS;
      default -> Items.FIRE_CHARGE;
    };
  }

  private static boolean needsHeldWeapon(ObjectiveType attackStyle) {
    return attackStyle == ObjectiveType.BOW_ATTACK
        || attackStyle == ObjectiveType.CROSSBOW_ATTACK
        || attackStyle == ObjectiveType.GUN_ATTACK;
  }

  private static boolean isHoldingWeaponFor(ObjectiveType attackStyle, LivingEntity livingEntity) {
    return switch (attackStyle) {
      case BOW_ATTACK -> AttackHandler.isHoldingBowWeapon(livingEntity);
      case CROSSBOW_ATTACK -> AttackHandler.isHoldingCrossbowWeapon(livingEntity);
      case GUN_ATTACK -> AttackHandler.isHoldingGunWeapon(livingEntity);
      default -> true;
    };
  }

  @Override
  public void init() {
    super.init();

    this.attackObjectiveButton.active = false;

    int left = this.contentLeftPos + 5;
    int top = this.contentTopPos + 2;

    this.howSectionTop = top;
    int attackStyleLeft = left;
    for (ObjectiveType attackStyle : ATTACK_STYLES) {
      ItemToggleButton attackStyleButton =
          this.addRenderableWidget(
              new ItemToggleButton(
                  attackStyleLeft,
                  top + 11,
                  new ItemStack(getAttackStyleIcon(attackStyle)),
                  TextComponent.getTranslatedConfigText(attackStyle.getObjectiveName()),
                  this.objectiveDataSet.hasObjective(attackStyle),
                  itemToggleButton ->
                      this.onAttackStyleChanged(attackStyle, itemToggleButton.selected())));
      attackStyleButton.setTooltip(
          Tooltip.create(
              TextComponent.getTranslatedConfigText(attackStyle.getObjectiveName())
                  .append("\n")
                  .append(
                      TextComponent.getTranslatedConfigText(
                          attackStyle.getObjectiveName() + ".tooltip"))));
      this.attackStyleButtons.put(attackStyle, attackStyleButton);
      attackStyleLeft += ItemToggleButton.SIZE + 4;
    }

    this.addRenderableWidget(
        new TextButton(
            attackStyleLeft + 6,
            top + 13,
            110,
            "projectile_settings",
            onPress ->
                NetworkMessageHandlerManager.getServerHandler()
                    .openConfiguration(
                        this.getEasyNPCUUID(), ConfigurationType.PROJECTILE_ATTRIBUTE)));

    this.whoSectionTop = top + 60;
    int whoRowTop = this.whoSectionTop + 11;
    int spareColumn = left + 105;
    int thirdColumn = left + 205;

    this.attackPlayerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                left,
                whoRowTop,
                "attack_target_player",
                this.objectiveDataSet.hasObjective(ObjectiveType.ATTACK_PLAYER)
                    || this.objectiveDataSet.hasObjective(
                        ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER),
                checkbox -> this.applyPlayerTarget()));
    this.spareOwnerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                spareColumn,
                whoRowTop,
                "attack_target_spare_owner",
                this.objectiveDataSet.hasObjective(ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER),
                checkbox -> this.applyPlayerTarget()));
    this.attackAnimalCheckbox =
        this.addRenderableWidget(
            this.getTargetCheckbox(
                thirdColumn, whoRowTop, "attack_target_animal", ObjectiveType.ATTACK_ANIMAL));

    whoRowTop += ROW_HEIGHT;
    this.attackVillagerCheckbox =
        this.addRenderableWidget(
            this.getTargetCheckbox(
                left, whoRowTop, "attack_target_villager", ObjectiveType.ATTACK_VILLAGER));
    this.attackMonsterCheckbox =
        this.addRenderableWidget(
            this.getTargetCheckbox(
                thirdColumn, whoRowTop, "attack_target_monster", ObjectiveType.ATTACK_MONSTER));

    whoRowTop += ROW_HEIGHT;
    this.attackMobCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                left,
                whoRowTop,
                "attack_target_mob",
                this.objectiveDataSet.hasObjective(ObjectiveType.ATTACK_MOB)
                    || this.objectiveDataSet.hasObjective(ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER),
                checkbox -> this.applyMobTarget()));
    this.spareCreeperCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                spareColumn,
                whoRowTop,
                "attack_target_spare_creeper",
                this.objectiveDataSet.hasObjective(ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER),
                checkbox -> this.applyMobTarget()));

    this.whenSectionTop = whoRowTop + ROW_HEIGHT + 5;
    int whenRowTop = this.whenSectionTop + 11;
    int secondColumn = left + 160;

    this.hurtByTargetCheckbox =
        this.addRenderableWidget(
            this.getDefenseCheckbox(left, whenRowTop, ObjectiveType.HURT_BY_TARGET, true));
    this.ownerHurtByTargetCheckbox =
        this.addRenderableWidget(
            this.getDefenseCheckbox(
                secondColumn, whenRowTop, ObjectiveType.OWNER_HURT_BY_TARGET, this.hasOwner()));

    whenRowTop += ROW_HEIGHT;
    this.factionHurtByTargetCheckbox =
        this.addRenderableWidget(
            this.getDefenseCheckbox(
                left, whenRowTop, ObjectiveType.FACTION_HURT_BY_TARGET, this.hasFaction()));
    this.ownerHurtTargetCheckbox =
        this.addRenderableWidget(
            this.getDefenseCheckbox(
                secondColumn, whenRowTop, ObjectiveType.OWNER_HURT_TARGET, this.hasOwner()));

    this.updateSpareCheckboxStates();
    this.updateAttackSummary();
  }

  private Checkbox getTargetCheckbox(
      int left, int top, String label, ObjectiveType objectiveType) {
    return new Checkbox(
        left,
        top,
        label,
        this.objectiveDataSet.hasObjective(objectiveType),
        checkbox -> this.setObjective(objectiveType, checkbox.selected()));
  }

  private Checkbox getDefenseCheckbox(
      int left, int top, ObjectiveType objectiveType, boolean requirementMet) {
    Checkbox checkbox =
        new Checkbox(
            left,
            top,
            "defend_when_" + objectiveType.getObjectiveName(),
            this.objectiveDataSet.hasObjective(objectiveType),
            changedCheckbox -> this.setObjective(objectiveType, changedCheckbox.selected()));
    checkbox.active = requirementMet;
    checkbox.setTooltip(
        Tooltip.create(
            TextComponent.getTranslatedConfigText(
                objectiveType.getObjectiveName() + (requirementMet ? ".tooltip" : ".disabled"))));
    return checkbox;
  }

  private void onAttackStyleChanged(ObjectiveType attackStyle, boolean selected) {
    this.setObjective(attackStyle, selected);
    if (selected) {
      ObjectiveType exclusiveAttackStyle = this.getExclusiveAttackStyle(attackStyle);
      ItemToggleButton exclusiveButton = this.attackStyleButtons.get(exclusiveAttackStyle);
      if (exclusiveButton != null && exclusiveButton.selected()) {
        exclusiveButton.setSelected(false);
        this.setObjective(exclusiveAttackStyle, false);
      }
    }
    this.updateAttackSummary();
  }

  private ObjectiveType getExclusiveAttackStyle(ObjectiveType attackStyle) {
    return switch (attackStyle) {
      case MELEE_ATTACK -> ObjectiveType.ZOMBIE_ATTACK;
      case ZOMBIE_ATTACK -> ObjectiveType.MELEE_ATTACK;
      default -> ObjectiveType.NONE;
    };
  }

  private void applyPlayerTarget() {
    boolean attackPlayer = this.attackPlayerCheckbox.selected();
    boolean spareOwner = this.spareOwnerCheckbox.selected();
    this.setObjective(ObjectiveType.ATTACK_PLAYER, attackPlayer && !spareOwner);
    this.setObjective(ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER, attackPlayer && spareOwner);
    this.updateSpareCheckboxStates();
  }

  private void applyMobTarget() {
    boolean attackMob = this.attackMobCheckbox.selected();
    boolean spareCreeper = this.spareCreeperCheckbox.selected();
    this.setObjective(ObjectiveType.ATTACK_MOB, attackMob && !spareCreeper);
    this.setObjective(ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER, attackMob && spareCreeper);
    this.updateSpareCheckboxStates();
  }

  private void updateSpareCheckboxStates() {
    this.spareOwnerCheckbox.active = this.attackPlayerCheckbox.selected() && this.hasOwner();
    this.spareOwnerCheckbox.setTooltip(
        Tooltip.create(
            TextComponent.getTranslatedConfigText(
                this.hasOwner()
                    ? "attack_target_spare_owner.tooltip"
                    : "attack_target_spare_owner.disabled")));
    this.spareCreeperCheckbox.active = this.attackMobCheckbox.selected();
  }

  private void setObjective(ObjectiveType objectiveType, boolean enabled) {
    if (enabled == this.objectiveDataSet.hasObjective(objectiveType)) {
      return;
    }

    ObjectiveDataEntry objectiveDataEntry =
        this.objectiveDataSet.getOrCreateObjective(objectiveType);
    objectiveDataEntry.setSpeedModifier(DEFAULT_SPEED_MODIFIER);
    if (enabled) {
      NetworkMessageHandlerManager.getServerHandler()
          .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
      this.objectiveDataSet.addObjective(objectiveDataEntry);
    } else {
      NetworkMessageHandlerManager.getServerHandler()
          .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
      this.objectiveDataSet.removeObjective(objectiveType);
    }
  }

  private void updateAttackSummary() {
    List<ObjectiveType> selectedAttackStyles = new ArrayList<>();
    for (ObjectiveType attackStyle : ATTACK_STYLES) {
      if (this.attackStyleButtons.get(attackStyle).selected()) {
        selectedAttackStyles.add(attackStyle);
      }
    }

    this.attackSummaryLines =
        this.font.split(this.getAttackSummary(selectedAttackStyles), CONTENT_WIDTH);
    Component attackWarning = this.getAttackWarning(selectedAttackStyles);
    this.attackWarningLines =
        attackWarning == null
            ? Collections.emptyList()
            : this.font.split(attackWarning, CONTENT_WIDTH);
  }

  private Component getAttackSummary(List<ObjectiveType> selectedAttackStyles) {
    if (selectedAttackStyles.isEmpty()) {
      return TextComponent.getTranslatedConfigText("attack_summary.none");
    }

    MutableComponent attackStyleList = TextComponent.getBlankText();
    for (int i = 0; i < selectedAttackStyles.size(); i++) {
      if (i > 0) {
        attackStyleList.append(
            TextComponent.getTranslatedConfigText(
                i == selectedAttackStyles.size() - 1
                    ? "attack_summary.and"
                    : "attack_summary.separator"));
      }
      attackStyleList.append(
          TextComponent.getTranslatedConfigText(
              selectedAttackStyles.get(i).getObjectiveName() + ".summary"));
    }
    return TextComponent.getTranslatedConfigText("attack_summary", attackStyleList);
  }

  private Component getAttackWarning(List<ObjectiveType> selectedAttackStyles) {
    LivingEntity livingEntity = this.getEasyNPC().getLivingEntity();
    ObjectiveType firstWeaponAttackStyle = null;
    for (ObjectiveType attackStyle : selectedAttackStyles) {
      if (!needsHeldWeapon(attackStyle)) {
        continue;
      }

      if (isHoldingWeaponFor(attackStyle, livingEntity)) {
        return null;
      }

      if (firstWeaponAttackStyle == null) {
        firstWeaponAttackStyle = attackStyle;
      }
    }

    if (firstWeaponAttackStyle == null) {
      return null;
    }

    return TextComponent.getTranslatedConfigText(
        firstWeaponAttackStyle.getObjectiveName() + ".missing_weapon");
  }

  private boolean hasOwner() {
    OwnerDataCapable<?> ownerData = this.getEasyNPC().getEasyNPCOwnerData();
    return ownerData != null && ownerData.hasNPCOwner();
  }

  private boolean hasFaction() {
    return !this.getAdditionalScreenData()
        .getData()
        .getString(FactionDataCapable.DATA_FACTION_NAME_TAG)
        .isEmpty();
  }

  private void renderSectionTitle(GuiGraphics guiGraphics, String translationKey, int top) {
    int left = this.contentLeftPos + 5;
    Component title = TextComponent.getTranslatedConfigText(translationKey);
    Text.drawString(guiGraphics, this.font, title, left, top, Constants.FONT_COLOR_DEFAULT);
    int lineLeft = left + this.font.width(title) + 4;
    guiGraphics.fill(
        lineLeft, top + 4, left + CONTENT_WIDTH, top + 5, SECTION_LINE_COLOR);
  }

  private int renderLines(
      GuiGraphics guiGraphics, List<FormattedCharSequence> lines, int top, int color) {
    int lineTop = top;
    for (FormattedCharSequence line : lines) {
      Text.drawString(guiGraphics, this.font, line, this.contentLeftPos + 5, lineTop, color);
      lineTop += this.font.lineHeight + 2;
    }
    return lineTop;
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    if (this.attackStyleButtons.isEmpty()) {
      return;
    }

    this.renderSectionTitle(guiGraphics, "attack_question_how", this.howSectionTop);
    int textTop =
        this.renderLines(
            guiGraphics,
            this.attackSummaryLines,
            this.howSectionTop + 11 + ItemToggleButton.SIZE + 4,
            SUMMARY_COLOR);
    this.renderLines(guiGraphics, this.attackWarningLines, textTop, WARNING_COLOR);
    this.renderSectionTitle(guiGraphics, "attack_question_who", this.whoSectionTop);
    this.renderSectionTitle(guiGraphics, "attack_question_when", this.whenSectionTop);
  }
}
