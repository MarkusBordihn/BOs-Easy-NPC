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

package de.markusbordihn.easynpc.client.screen.configuration.objective;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class AttackObjectiveConfigurationScreen<T extends ConfigurationMenu>
    extends ObjectiveConfigurationScreen<T> {

  protected Checkbox meleeAttackCheckbox;
  protected Checkbox zombieAttackCheckbox;
  protected Checkbox crossbowAttackCheckbox;
  protected Checkbox bowAttackCheckbox;
  protected Checkbox gunAttackCheckbox;
  protected Checkbox attackAnimalCheckbox;
  protected Checkbox attackPlayerCheckbox;
  protected Checkbox attackPlayerWithoutOwnerCheckbox;
  protected Checkbox attackMonsterCheckbox;
  protected Checkbox attackMobCheckbox;
  protected Checkbox attackMobWithoutCreeperCheckbox;
  protected Checkbox attackVillagerCheckbox;

  public AttackObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.attackObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 15;
    int objectiveEntriesFirstColumn = this.contentLeftPos + 5;
    int objectiveEntriesSecondColumn = this.contentLeftPos + 145;

    // Melee Attacks
    this.meleeAttackCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.MELEE_ATTACK));

    // Zombie Attack
    this.zombieAttackCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn, objectiveEntriesTop, ObjectiveType.ZOMBIE_ATTACK));

    // Crossbow Attack
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.crossbowAttackCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.CROSSBOW_ATTACK));

    // Bow Attack
    this.bowAttackCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn, objectiveEntriesTop, ObjectiveType.BOW_ATTACK));

    // Gun Attack
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.gunAttackCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.GUN_ATTACK));

    // Attack Player
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES + 10;
    this.attackPlayerCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.ATTACK_PLAYER));

    // Attack Player (w/o Owner)
    this.attackPlayerWithoutOwnerCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER));

    // Attack Villager
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.attackVillagerCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.ATTACK_VILLAGER));

    // Attack Animal
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.attackAnimalCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.ATTACK_ANIMAL));

    // Attack Monster
    this.attackMonsterCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn, objectiveEntriesTop, ObjectiveType.ATTACK_MONSTER));

    // Attack Mob
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.attackMobCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.ATTACK_MOB));

    // Attack Mob w/o Creeper
    this.attackMobWithoutCreeperCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesSecondColumn,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER));
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    if (this.meleeAttackCheckbox != null) {
      int y = this.meleeAttackCheckbox.y - 3;
      this.fillGradient(
          poseStack,
          this.contentLeftPos + 5,
          y,
          this.contentLeftPos + 300,
          y + 1,
          0x60808080,
          0x60808080);
      Text.drawConfigString(
          poseStack, this.font, "attack_types", this.contentLeftPos + 115, y - 8, 0xFF808080);
    }

    if (this.attackPlayerCheckbox != null) {
      int y = this.attackPlayerCheckbox.y - 3;
      this.fillGradient(
          poseStack,
          this.contentLeftPos + 5,
          y,
          this.contentLeftPos + 300,
          y + 1,
          0x60808080,
          0x60808080);
      Text.drawConfigString(
          poseStack, this.font, "attack_targets", this.contentLeftPos + 115, y - 8, 0xFF808080);
    }
  }
}
