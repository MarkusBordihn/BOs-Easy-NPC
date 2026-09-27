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

import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
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
  protected Checkbox followItemCheckbox;
  protected EditBox followItemId;
  protected Button followItemIdSaveButton;

  private String savedPlayerName;
  private String savedEntityUUID;
  private String savedItemTag;

  public FollowObjectiveConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    this.followObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 5;
    int objectiveEntriesFirstColumn = this.contentLeftPos + 5;
    int objectiveEntriesSecondColumn = this.contentLeftPos + 145;

    OwnerDataCapable<?> ownerData = this.getOwnerData();
    this.followOwnerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.FOLLOW_OWNER.getObjectiveName(),
                ownerData.getNPCOwnerName(),
                this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_OWNER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER);
                  objectiveDataEntry.setTargetOwnerUUID(ownerData.getOwnerUUID());
                  if (checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  } else {
                    NetworkMessageHandlerManager.getServerHandler()
                        .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  }
                }));

    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.savedPlayerName =
        this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_PLAYER)
            ? this.objectiveDataSet.getObjective(ObjectiveType.FOLLOW_PLAYER).getTargetPlayerName()
            : "";
    this.followPlayerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.FOLLOW_PLAYER.getObjectiveName(),
                this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_PLAYER),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_PLAYER);
                  if (this.followPlayerName != null) {
                    objectiveDataEntry.setTargetPlayerName(this.followPlayerName.getValue());
                    this.followPlayerName.setEditable(checkbox.selected());
                  }
                  if (this.followPlayerNameSaveButton != null) {
                    this.followPlayerNameSaveButton.active =
                        checkbox.selected()
                            && this.followPlayerName != null
                            && !this.followPlayerName.getValue().equals(this.savedPlayerName);
                  }
                  if (!checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  } else if (!this.followPlayerName.getValue().isEmpty()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  }
                }));
    this.followOwnerCheckbox.active = ownerData.hasNPCOwner();
    this.followPlayerName =
        this.addRenderableWidget(
            new TextField(this.font, objectiveEntriesSecondColumn, objectiveEntriesTop, 125));
    this.followPlayerName.setEditable(
        this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_PLAYER));
    this.followPlayerName.setResponder(
        value -> {
          if (this.followPlayerNameSaveButton != null) {
            this.followPlayerNameSaveButton.active =
                this.followPlayerCheckbox != null
                    && this.followPlayerCheckbox.selected()
                    && value != null
                    && !value.equals(this.savedPlayerName);
          }
        });
    this.followPlayerName.setValue(this.savedPlayerName);
    this.followPlayerNameSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.followPlayerName.getX() + this.followPlayerName.getWidth() + 5,
                objectiveEntriesTop - 1,
                onPress -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_PLAYER);
                  objectiveDataEntry.setTargetPlayerName(this.followPlayerName.getValue());
                  NetworkMessageHandlerManager.getServerHandler()
                      .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  this.savedPlayerName = this.followPlayerName.getValue();
                  this.followPlayerNameSaveButton.active = false;
                }));
    this.followPlayerNameSaveButton.active = false;

    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.savedEntityUUID =
        this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID)
                && this.objectiveDataSet
                        .getObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID)
                        .getTargetEntityUUID()
                    != null
            ? this.objectiveDataSet
                .getObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID)
                .getTargetEntityUUID()
                .toString()
            : "";
    this.followEntityCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.FOLLOW_ENTITY_BY_UUID.getObjectiveName(),
                this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_ENTITY_BY_UUID);
                  if (this.followEntityUUID != null) {
                    if (!this.followEntityUUID.getValue().isEmpty()) {
                      UUID entityUUID = null;
                      try {
                        entityUUID = UUID.fromString(this.followEntityUUID.getValue());
                      } catch (IllegalArgumentException e) {
                        log.error(
                            "Unable to parse UUID {} for {}",
                            this.followEntityUUID.getValue(),
                            this.getEasyNPCUUID());
                      }
                      if (entityUUID != null) {
                        objectiveDataEntry.setTargetEntityUUID(entityUUID);
                      }
                    }
                    this.followEntityUUID.setEditable(checkbox.selected());
                  }
                  if (this.followEntityUUIDSaveButton != null) {
                    this.followEntityUUIDSaveButton.active =
                        checkbox.selected()
                            && this.followEntityUUID != null
                            && !this.followEntityUUID.getValue().equals(this.savedEntityUUID);
                  }
                  if (!checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  } else if (!this.followEntityUUID.getValue().isEmpty()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  }
                }));
    this.followEntityUUID =
        this.addRenderableWidget(
            new TextField(this.font, objectiveEntriesSecondColumn, objectiveEntriesTop, 125));
    this.followEntityUUID.setMaxLength(36);
    this.followEntityUUID.setEditable(
        this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID));
    this.followEntityUUID.setResponder(
        value -> {
          if (this.followEntityUUIDSaveButton != null) {
            this.followEntityUUIDSaveButton.active =
                this.followEntityCheckbox != null
                    && this.followEntityCheckbox.selected()
                    && value != null
                    && !value.equals(this.savedEntityUUID);
          }
        });
    this.followEntityUUID.setValue(this.savedEntityUUID);
    this.followEntityUUIDSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.followEntityUUID.getX() + this.followEntityUUID.getWidth() + 5,
                objectiveEntriesTop - 1,
                onPress -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_ENTITY_BY_UUID);
                  objectiveDataEntry.setTargetEntityUUID(
                      !this.followEntityUUID.getValue().isEmpty()
                          ? UUID.fromString(this.followEntityUUID.getValue())
                          : null);
                  NetworkMessageHandlerManager.getServerHandler()
                      .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  this.savedEntityUUID = this.followEntityUUID.getValue();
                  this.followEntityUUIDSaveButton.active = false;
                }));
    this.followEntityUUIDSaveButton.active = false;

    objectiveEntriesTop += SPACE_BETWEEN_ENTRIES;
    this.savedItemTag =
        this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_ITEM)
                && this.objectiveDataSet.getObjective(ObjectiveType.FOLLOW_ITEM).getTargetItemTag()
                    != null
            ? this.objectiveDataSet.getObjective(ObjectiveType.FOLLOW_ITEM).getTargetItemTag()
            : "";
    this.followItemCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                objectiveEntriesFirstColumn,
                objectiveEntriesTop,
                ObjectiveType.FOLLOW_ITEM.getObjectiveName(),
                this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_ITEM),
                checkbox -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_ITEM);
                  if (this.followItemId != null) {
                    objectiveDataEntry.setTargetItemTag(this.followItemId.getValue());
                    this.followItemId.setEditable(checkbox.selected());
                  }
                  if (this.followItemIdSaveButton != null) {
                    this.followItemIdSaveButton.active =
                        checkbox.selected()
                            && this.followItemId != null
                            && !this.followItemId.getValue().equals(this.savedItemTag);
                  }
                  if (!checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .removeObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  } else if (!this.followItemId.getValue().isEmpty()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  }
                }));
    this.followItemId =
        this.addRenderableWidget(
            new TextField(this.font, objectiveEntriesSecondColumn, objectiveEntriesTop, 125));
    this.followItemId.setEditable(this.objectiveDataSet.hasObjective(ObjectiveType.FOLLOW_ITEM));
    this.followItemId.setResponder(
        value -> {
          if (this.followItemIdSaveButton != null) {
            this.followItemIdSaveButton.active =
                this.followItemCheckbox != null
                    && this.followItemCheckbox.selected()
                    && value != null
                    && !value.equals(this.savedItemTag);
          }
        });
    this.followItemId.setValue(this.savedItemTag);
    this.followItemIdSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.followItemId.getX() + this.followItemId.getWidth() + 5,
                objectiveEntriesTop - 1,
                onPress -> {
                  ObjectiveDataEntry objectiveDataEntry =
                      new ObjectiveDataEntry(ObjectiveType.FOLLOW_ITEM);
                  objectiveDataEntry.setTargetItemTag(this.followItemId.getValue());
                  NetworkMessageHandlerManager.getServerHandler()
                      .addOrUpdateObjective(this.getEasyNPCUUID(), objectiveDataEntry);
                  this.savedItemTag = this.followItemId.getValue();
                  this.followItemIdSaveButton.active = false;
                }));
    this.followItemIdSaveButton.active = false;
  }
}
