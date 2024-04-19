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

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.menu.configuration.objective.AttackObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class AttackObjectiveConfigurationScreen
    extends ObjectiveConfigurationScreen<AttackObjectiveConfigurationMenu> {

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
  protected Checkbox attackEntityByUUIDCheckbox;
  protected EditBox attackEntityByUUIDEditBox;
  protected Button attackEntityByUUIDSaveButton;

  public AttackObjectiveConfigurationScreen(
      AttackObjectiveConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.attackObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 5;

    // Melee Attacks
    this.meleeAttackCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.MELEE_ATTACK.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.MELEE_ATTACK),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.MELEE_ATTACK, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    this.zombieAttackCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 150,
                objectiveEntriesTop,
                ObjectiveType.ZOMBIE_ATTACK.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.ZOMBIE_ATTACK),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.ZOMBIE_ATTACK, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Crossbow Attack
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.crossbowAttackCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.CROSSBOW_ATTACK.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.CROSSBOW_ATTACK),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.CROSSBOW_ATTACK, 4);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Bow Attack
    this.bowAttackCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 150,
                objectiveEntriesTop,
                ObjectiveType.BOW_ATTACK.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.BOW_ATTACK),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.BOW_ATTACK, 4);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Gun Attack
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.gunAttackCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.GUN_ATTACK.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.GUN_ATTACK),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.GUN_ATTACK, 4);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Attack Player
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES + 10;
    this.attackPlayerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_PLAYER.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.ATTACK_PLAYER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.ATTACK_PLAYER, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Attack Player (w/o Owner)
    this.attackPlayerWithoutOwnerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 150,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(
                          ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Attack Villager
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.attackVillagerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_VILLAGER.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.ATTACK_VILLAGER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.ATTACK_VILLAGER, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Attack Animal
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.attackAnimalCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_ANIMAL.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.ATTACK_ANIMAL),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.ATTACK_ANIMAL, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Attack Monster
    this.attackMonsterCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 150,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_MONSTER.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.ATTACK_MONSTER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.ATTACK_MONSTER, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Attack Mob
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.attackMobCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_MOB.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.ATTACK_MOB),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(ObjectiveType.ATTACK_MOB, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Attack Mob w/o Creeper
    this.attackMobWithoutCreeperCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 150,
                objectiveEntriesTop,
                ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      objectiveDataSet.getOrCreateObjective(
                          ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER, 2);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    if (this.meleeAttackCheckbox != null) {
      int y = this.meleeAttackCheckbox.getY() - 3;
      guiGraphics.fillGradient(
          this.contentLeftPos + 5, y, this.contentLeftPos + 300, y + 1, 0x60808080, 0x60808080);
      Text.drawConfigString(
          guiGraphics, this.font, "attack_types", this.contentLeftPos + 115, y - 8, 0xFF808080);
    }

    if (this.attackPlayerCheckbox != null) {
      int y = this.attackPlayerCheckbox.getY() - 3;
      guiGraphics.fillGradient(
          this.contentLeftPos + 5, y, this.contentLeftPos + 300, y + 1, 0x60808080, 0x60808080);
      Text.drawConfigString(
          guiGraphics, this.font, "attack_targets", this.contentLeftPos + 115, y - 8, 0xFF808080);
    }
  }
}
