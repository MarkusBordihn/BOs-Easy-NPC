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
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.UUID;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class LookObjectiveConfigurationScreen<T extends ConfigurationMenu>
    extends ObjectiveConfigurationScreen<T> {

  protected Checkbox lookResetCheckbox;
  protected Checkbox randomLookAroundCheckbox;
  protected Checkbox lookAtOwnerCheckbox;
  protected Checkbox lookAtPlayerCheckbox;
  protected Checkbox lookAtMobCheckbox;
  protected Checkbox lookAtAnimalCheckbox;
  protected Checkbox lookAtEntityCheckbox;
  protected EditBox lookAtEntityUUID;
  protected Button lookAtEntityUUIDSaveButton;

  public LookObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.lookObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 5;
    int objectiveEntriesFirstColumn = this.contentLeftPos + 5;
    int objectiveEntriesSecondColumn = this.contentLeftPos + 145;

    // Look Reset
    this.lookResetCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.LOOK_AT_RESET));

    // Random Look Around
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.randomLookAroundCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.LOOK_RANDOM_AROUND));

    // Look at Owner
    OwnerData<?> ownerData = this.getOwnerData();
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtOwnerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.LOOK_AT_OWNER.getObjectiveName(),
                ownerData.getOwnerName(),
                objectiveDataSet.hasObjective(ObjectiveType.LOOK_AT_OWNER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_AT_OWNER, 9);
                  objectiveDataEntry.setTargetOwnerUUID(ownerData.getOwnerUUID());
                  if (checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  } else {
                    NetworkMessageHandlerManager.getServerHandler()
                        .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  }
                }));
    this.lookAtOwnerCheckbox.active = false;

    // Look at Player
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtPlayerCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.LOOK_AT_PLAYER));

    // Look at Mob
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtMobCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.LOOK_AT_MOB));

    // Look at Animal
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtAnimalCheckbox =
        this.addRenderableWidget(
            this.getObjectiveCheckbox(
                objectiveEntriesFirstColumn, objectiveEntriesTop, ObjectiveType.LOOK_AT_ANIMAL));

    // Look Entity with UUID input field
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.lookAtEntityCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
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
                            "Unable to parse UUID {} for {}",
                            lookAtEntityUUID.getValue(),
                            this.getEasyNPCUUID());
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
                    NetworkMessageHandlerManager.getServerHandler()
                        .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  } else if (!lookAtEntityUUID.getValue().isEmpty()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  }
                }));
    this.lookAtEntityCheckbox.active = false;
    this.lookAtEntityUUID =
        this.addRenderableWidget(
            new TextField(this.font, objectiveEntriesSecondColumn, objectiveEntriesTop, 115));
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
                this.lookAtEntityUUID.getX() + this.lookAtEntityUUID.getWidth() + 5,
                objectiveEntriesTop - 1,
                onPress -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.LOOK_AT_ENTITY_BY_UUID, 9);
                  objectiveDataEntry.setTargetEntityUUID(
                      !lookAtEntityUUID.getValue().isEmpty()
                          ? UUID.fromString(lookAtEntityUUID.getValue())
                          : null);
                  NetworkMessageHandlerManager.getServerHandler()
                      .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                }));
    this.lookAtEntityUUIDSaveButton.active = false;
  }
}
