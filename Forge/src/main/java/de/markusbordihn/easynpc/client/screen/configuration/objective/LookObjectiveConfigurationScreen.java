/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the looking conditions:
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
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.menu.configuration.objective.LookObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import java.util.UUID;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class LookObjectiveConfigurationScreen
    extends ObjectiveConfigurationScreen<LookObjectiveConfigurationMenu> {

  protected final OwnerData<?> ownerData;
  protected Checkbox lookResetCheckbox;
  protected Checkbox randomLookAroundCheckbox;
  protected Checkbox lookAtOwnerCheckbox;
  protected Checkbox lookAtPlayerCheckbox;
  protected Checkbox lookAtMobCheckbox;
  protected Checkbox lookAtAnimalCheckbox;
  protected Checkbox lookAtEntityCheckbox;
  protected EditBox lookAtEntityUUID;
  protected Button lookAtEntityUUIDSaveButton;

  public LookObjectiveConfigurationScreen(
      LookObjectiveConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.ownerData = this.easyNPC.getEasyNPCOwnerData();
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.lookObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 5;

    // Look Reset
    this.lookResetCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.LOOK_AT_RESET.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.LOOK_AT_RESET),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_AT_RESET, 9);
                  if (checkbox.selected()) {
                    ServerNetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    ServerNetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Random Look Around
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.randomLookAroundCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.LOOK_RANDOM_AROUND.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.LOOK_RANDOM_AROUND),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_RANDOM_AROUND, 10);
                  if (checkbox.selected()) {
                    ServerNetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    ServerNetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Look at Owner
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtOwnerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.LOOK_AT_OWNER.getObjectiveName(),
                this.ownerData.getOwnerName(),
                objectiveDataSet.hasObjective(ObjectiveType.LOOK_AT_OWNER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_AT_OWNER, 9);
                  objectiveDataEntry.setTargetOwnerUUID(this.ownerData.getOwnerUUID());
                  if (checkbox.selected()) {
                    ServerNetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    ServerNetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));
    this.lookAtOwnerCheckbox.active = false;

    // Look at Player
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtPlayerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.LOOK_AT_PLAYER.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.LOOK_AT_PLAYER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_AT_PLAYER, 9);
                  if (checkbox.selected()) {
                    ServerNetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    ServerNetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Look at Mob
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtMobCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.LOOK_AT_MOB.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.LOOK_AT_MOB),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_AT_MOB, 10);
                  if (checkbox.selected()) {
                    ServerNetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    ServerNetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Look at Animal
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtAnimalCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.LOOK_AT_ANIMAL.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.LOOK_AT_ANIMAL),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_AT_ANIMAL, 10);
                  if (checkbox.selected()) {
                    ServerNetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  } else {
                    ServerNetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  }
                }));

    // Look Entity with UUID input field
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtEntityCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                ObjectiveType.LOOK_AT_ENTITY_BY_UUID.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.LOOK_AT_ENTITY_BY_UUID),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_AT_ENTITY_BY_UUID, 9);
                  if (lookAtEntityUUID != null) {
                    if (!lookAtEntityUUID.getValue().isEmpty()) {
                      UUID entityUUID = null;
                      try {
                        entityUUID = UUID.fromString(lookAtEntityUUID.getValue());
                      } catch (IllegalArgumentException e) {
                        log.error(
                            "Unable to parse UUID {} for {}", lookAtEntityUUID.getValue(), uuid);
                      }
                      if (entityUUID != null) {
                        objectiveDataEntry.setTargetEntityUUID(entityUUID);
                      }
                    }
                    lookAtEntityUUID.setEditable(checkbox.selected());
                  }
                  if (lookAtEntityUUIDSaveButton != null) {
                    lookAtEntityUUIDSaveButton.active = checkbox.selected();
                  }
                  if (!checkbox.selected()) {
                    ServerNetworkMessageHandler.removeObjective(uuid, objectiveDataEntry);
                  } else if (!lookAtEntityUUID.getValue().isEmpty()) {
                    ServerNetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                  }
                }));
    this.lookAtEntityCheckbox.active = false;
    this.lookAtEntityUUID =
        this.addRenderableWidget(
            new TextField(this.font, this.contentLeftPos + 160, objectiveEntriesTop, 115));
    lookAtEntityUUID.setMaxLength(36);
    lookAtEntityUUID.setEditable(
        objectiveDataSet.hasObjective(ObjectiveType.LOOK_AT_ENTITY_BY_UUID));
    lookAtEntityUUID.setResponder(
        value -> {
          if (this.lookAtEntityUUIDSaveButton != null) {
            this.lookAtEntityUUIDSaveButton.active = value != null && !value.isEmpty();
          }
        });
    lookAtEntityUUID.setValue(
        objectiveDataSet.hasObjective(ObjectiveType.LOOK_AT_ENTITY_BY_UUID)
            && objectiveDataSet
            .getObjective(ObjectiveType.LOOK_AT_ENTITY_BY_UUID)
            .getTargetEntityUUID()
            != null
            ? objectiveDataSet
            .getObjective(ObjectiveType.LOOK_AT_ENTITY_BY_UUID)
            .getTargetEntityUUID()
            .toString()
            : "");
    this.lookAtEntityUUID.active = false;
    this.lookAtEntityUUIDSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.lookAtEntityUUID.x + this.lookAtEntityUUID.getWidth() + 5,
                objectiveEntriesTop - 1,
                onPress -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_AT_ENTITY_BY_UUID, 9);
                  objectiveDataEntry.setTargetEntityUUID(
                      !lookAtEntityUUID.getValue().isEmpty()
                          ? UUID.fromString(lookAtEntityUUID.getValue())
                          : null);
                  ServerNetworkMessageHandler.addObjective(uuid, objectiveDataEntry);
                }));
    this.lookAtEntityUUIDSaveButton.active = false;
  }
}
