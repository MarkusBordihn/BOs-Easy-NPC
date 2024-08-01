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

public class FollowObjectiveConfigurationScreen<T extends ConfigurationMenu>
    extends ObjectiveConfigurationScreen<T> {

  protected Checkbox followOwnerCheckbox;
  protected Checkbox followPlayerCheckbox;
  protected EditBox followPlayerName;
  protected Button followPlayerNameSaveButton;
  protected Checkbox followEntityCheckbox;
  protected EditBox followEntityUUID;
  protected Button followEntityUUIDSaveButton;

  public FollowObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.followObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 5;
    int objectiveEntriesFirstColumn = this.contentLeftPos + 5;
    int objectiveEntriesSecondColumn = this.contentLeftPos + 145;

    // Follow Owner
    OwnerData<?> ownerData = this.getOwnerData();
    this.followOwnerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.FOLLOW_OWNER.getObjectiveName(),
                ownerData.getOwnerName(),
                objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_OWNER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER, 6);
                  objectiveDataEntry.setTargetOwnerUUID(ownerData.getOwnerUUID());
                  if (checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .addObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  } else {
                    NetworkMessageHandlerManager.getServerHandler()
                        .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  }
                }));

    // Follow Player with name input field
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.followPlayerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.FOLLOW_PLAYER.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_PLAYER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_PLAYER, 7);
                  if (followPlayerName != null) {
                    objectiveDataEntry.setTargetPlayerName(followPlayerName.getValue());
                    followPlayerName.setEditable(checkbox.selected());
                  }
                  if (followPlayerNameSaveButton != null) {
                    followPlayerNameSaveButton.active = checkbox.selected();
                  }
                  if (!checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  } else if (!followPlayerName.getValue().isEmpty()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .addObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  }
                }));
    this.followOwnerCheckbox.active = ownerData.hasOwner();
    this.followPlayerName =
        this.addRenderableWidget(
            new TextField(this.font, objectiveEntriesSecondColumn, objectiveEntriesTop, 125));
    followPlayerName.setEditable(objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_PLAYER));
    followPlayerName.setResponder(
        value -> {
          if (this.followPlayerNameSaveButton != null) {
            this.followPlayerNameSaveButton.active = value != null && !value.isEmpty();
          }
        });
    followPlayerName.setValue(
        objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_PLAYER)
            ? objectiveDataSet.getObjective(ObjectiveType.FOLLOW_PLAYER).getTargetPlayerName()
            : "");
    this.followPlayerNameSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.followPlayerName.x + this.followPlayerName.getWidth() + 5,
                objectiveEntriesTop - 1,
                onPress -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_PLAYER);
                  objectiveDataEntry.setTargetPlayerName(this.followPlayerName.getValue());
                  NetworkMessageHandlerManager.getServerHandler()
                      .addObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                }));

    // Follow Entity with UUID input field
    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.followEntityCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.FOLLOW_ENTITY_BY_UUID.getObjectiveName(),
                objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_ENTITY_BY_UUID, 7);
                  if (followEntityUUID != null) {
                    if (!followEntityUUID.getValue().isEmpty()) {
                      UUID entityUUID = null;
                      try {
                        entityUUID = UUID.fromString(followEntityUUID.getValue());
                      } catch (IllegalArgumentException e) {
                        log.error(
                            "Unable to parse UUID {} for {}",
                            followEntityUUID.getValue(),
                            this.getEasyNPCUUID());
                      }
                      if (entityUUID != null) {
                        objectiveDataEntry.setTargetEntityUUID(entityUUID);
                      }
                    }
                    followEntityUUID.setEditable(checkbox.selected());
                  }
                  if (followEntityUUIDSaveButton != null) {
                    followEntityUUIDSaveButton.active = checkbox.selected();
                  }
                  if (!checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  } else if (!followEntityUUID.getValue().isEmpty()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .addObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  }
                }));
    this.followEntityUUID =
        this.addRenderableWidget(
            new TextField(this.font, objectiveEntriesSecondColumn, objectiveEntriesTop, 125));
    followEntityUUID.setMaxLength(36);
    followEntityUUID.setEditable(
        objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID));
    followEntityUUID.setResponder(
        value -> {
          if (this.followEntityUUIDSaveButton != null) {
            this.followEntityUUIDSaveButton.active = value != null && !value.isEmpty();
          }
        });
    followEntityUUID.setValue(
        objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID)
                && objectiveDataSet
                        .getObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID)
                        .getTargetEntityUUID()
                    != null
            ? objectiveDataSet
                .getObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID)
                .getTargetEntityUUID()
                .toString()
            : "");
    this.followEntityUUIDSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.followEntityUUID.x + this.followEntityUUID.getWidth() + 5,
                objectiveEntriesTop - 1,
                onPress -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_ENTITY_BY_UUID, 7);
                  objectiveDataEntry.setTargetEntityUUID(
                      !followEntityUUID.getValue().isEmpty()
                          ? UUID.fromString(followEntityUUID.getValue())
                          : null);
                  NetworkMessageHandlerManager.getServerHandler()
                      .addObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                }));
  }
}
