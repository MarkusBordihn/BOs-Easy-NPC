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
import de.markusbordihn.easynpc.data.objective.ObjectiveData;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.menu.configuration.objective.BasicObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import java.util.UUID;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class BasicObjectiveConfigurationScreen
    extends ObjectiveConfigurationScreen<BasicObjectiveConfigurationMenu> {

  // Objectives IDs
  private static final String RANDOM_STROLL = "random_stroll";
  private static final String WATER_AVOIDING_RANDOM_STROLL = "water_avoiding_random_stroll";
  private static final String FOLLOW_OWNER = "follow_owner";
  private static final String FOLLOW_PLAYER = "follow_player";
  private static final String FOLLOW_ENTITY_BY_UUID = "follow_entity_by_uuid";

  // Objectives

  // Basic Objective Checkbox
  protected Checkbox strollRandomAroundCheckbox;
  protected Checkbox waterAvoidingRandomStrollCheckbox;
  protected Checkbox followOwnerCheckbox;

  // Follow Player with name input field and save button
  protected Checkbox followPlayerCheckbox;
  protected EditBox followPlayerName;
  protected Button followPlayerNameSaveButton;

  // Follow entity with UUID input field and save button
  protected Checkbox followEntityCheckbox;
  protected EditBox followEntityUUID;
  protected Button followEntityUUIDSaveButton;

  public BasicObjectiveConfigurationScreen(
      BasicObjectiveConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicObjectiveButton.active = false;

    int objectiveEntriesTop = this.contentTopPos + 5;

    // Stroll Random Around
    this.strollRandomAroundCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                RANDOM_STROLL,
                objectiveDataSet.hasObjective(RANDOM_STROLL),
                checkbox -> {
                  ObjectiveData objectiveData = new ObjectiveData();
                  objectiveData.setId(RANDOM_STROLL);
                  objectiveData.setType(ObjectiveType.RANDOM_STROLL);
                  objectiveData.setPriority(5);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  }
                }));

    // Water Avoiding Random Stroll
    objectiveEntriesTop += 25;
    this.waterAvoidingRandomStrollCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                WATER_AVOIDING_RANDOM_STROLL,
                objectiveDataSet.hasObjective(WATER_AVOIDING_RANDOM_STROLL),
                checkbox -> {
                  ObjectiveData objectiveData = new ObjectiveData();
                  objectiveData.setId(WATER_AVOIDING_RANDOM_STROLL);
                  objectiveData.setType(ObjectiveType.WATER_AVOIDING_RANDOM_STROLL);
                  objectiveData.setPriority(5);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  }
                }));

    // Follow Owner
    objectiveEntriesTop += 25;
    this.followOwnerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                FOLLOW_OWNER,
                objectiveDataSet.hasObjective(FOLLOW_OWNER),
                checkbox -> {
                  ObjectiveData objectiveData = new ObjectiveData();
                  objectiveData.setId(FOLLOW_OWNER);
                  objectiveData.setType(ObjectiveType.FOLLOW_OWNER);
                  objectiveData.setPriority(6);
                  if (checkbox.selected()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  } else {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  }
                }));

    // Follow Player with name input field
    objectiveEntriesTop += 25;
    this.followPlayerCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                FOLLOW_PLAYER,
                objectiveDataSet.hasObjective(FOLLOW_PLAYER),
                checkbox -> {
                  ObjectiveData objectiveData = new ObjectiveData();
                  objectiveData.setId(FOLLOW_PLAYER);
                  objectiveData.setType(ObjectiveType.FOLLOW_PLAYER);
                  objectiveData.setPriority(7);
                  if (followPlayerName != null) {
                    objectiveData.setTargetPlayerName(followPlayerName.getValue());
                    followPlayerName.setEditable(checkbox.selected());
                  }
                  if (followPlayerNameSaveButton != null) {
                    followPlayerNameSaveButton.active = checkbox.selected();
                  }
                  if (!checkbox.selected()) {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  } else if (followPlayerName.getValue() != null
                      && !followPlayerName.getValue().isEmpty()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  }
                }));
    this.followPlayerName =
        this.addRenderableWidget(
            new TextField(this.font, this.contentLeftPos + 150, objectiveEntriesTop + 1, 125));
    followPlayerName.setEditable(objectiveDataSet.hasObjective(FOLLOW_PLAYER));
    followPlayerName.setResponder(
        value -> {
          if (this.followPlayerNameSaveButton != null) {
            this.followPlayerNameSaveButton.active = value != null && !value.isEmpty();
          }
        });
    followPlayerName.setValue(
        objectiveDataSet.hasObjective(FOLLOW_PLAYER)
            ? objectiveDataSet.getObjective(FOLLOW_PLAYER).getTargetPlayerName()
            : "");
    this.followPlayerNameSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.followPlayerName.x + this.followPlayerName.getWidth() + 5,
                objectiveEntriesTop,
                onPress -> {
                  ObjectiveData objectiveData = new ObjectiveData();
                  objectiveData.setId(FOLLOW_PLAYER);
                  objectiveData.setType(ObjectiveType.FOLLOW_PLAYER);
                  objectiveData.setPriority(7);
                  objectiveData.setTargetPlayerName(this.followPlayerName.getValue());
                  NetworkMessageHandler.addObjective(uuid, objectiveData);
                }));

    // Follow Entity with UUID input field
    objectiveEntriesTop += 25;
    this.followEntityCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                objectiveEntriesTop,
                FOLLOW_ENTITY_BY_UUID,
                objectiveDataSet.hasObjective(FOLLOW_ENTITY_BY_UUID),
                checkbox -> {
                  ObjectiveData objectiveData = new ObjectiveData();
                  objectiveData.setId(FOLLOW_ENTITY_BY_UUID);
                  objectiveData.setType(ObjectiveType.FOLLOW_ENTITY_BY_UUID);
                  objectiveData.setPriority(7);
                  if (followEntityUUID != null) {
                    if (followEntityUUID.getValue() != null) {
                      UUID entityUUID = null;
                      try {
                        entityUUID = UUID.fromString(followEntityUUID.getValue());
                      } catch (IllegalArgumentException e) {
                        log.error(
                            "Unable to parse UUID {} for {}", followEntityUUID.getValue(), uuid);
                      }
                      if (entityUUID != null) {
                        objectiveData.setTargetEntityUUID(entityUUID);
                      }
                    }
                    followEntityUUID.setEditable(checkbox.selected());
                  }
                  if (followEntityUUIDSaveButton != null) {
                    followEntityUUIDSaveButton.active = checkbox.selected();
                  }
                  if (!checkbox.selected()) {
                    NetworkMessageHandler.removeObjective(uuid, objectiveData);
                  } else if (followEntityUUID.getValue() != null
                      && !followEntityUUID.getValue().isEmpty()) {
                    NetworkMessageHandler.addObjective(uuid, objectiveData);
                  }
                }));
    this.followEntityUUID =
        this.addRenderableWidget(
            new TextField(this.font, this.contentLeftPos + 150, objectiveEntriesTop + 1, 125));
    followEntityUUID.setMaxLength(36);
    followEntityUUID.setEditable(objectiveDataSet.hasObjective(FOLLOW_ENTITY_BY_UUID));
    followEntityUUID.setResponder(
        value -> {
          if (this.followEntityUUIDSaveButton != null) {
            this.followEntityUUIDSaveButton.active = value != null && !value.isEmpty();
          }
        });
    followEntityUUID.setValue(
        objectiveDataSet.hasObjective(FOLLOW_ENTITY_BY_UUID)
                && objectiveDataSet.getObjective(FOLLOW_ENTITY_BY_UUID).getTargetEntityUUID()
                    != null
            ? objectiveDataSet.getObjective(FOLLOW_ENTITY_BY_UUID).getTargetEntityUUID().toString()
            : "");
    this.followEntityUUIDSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.followEntityUUID.x + this.followEntityUUID.getWidth() + 5,
                objectiveEntriesTop,
                onPress -> {
                  ObjectiveData objectiveData = new ObjectiveData();
                  objectiveData.setId(FOLLOW_ENTITY_BY_UUID);
                  objectiveData.setType(ObjectiveType.FOLLOW_ENTITY_BY_UUID);
                  objectiveData.setPriority(7);
                  objectiveData.setTargetEntityUUID(
                      followEntityUUID.getValue() != null
                          ? UUID.fromString(followEntityUUID.getValue())
                          : null);
                  NetworkMessageHandler.addObjective(uuid, objectiveData);
                }));

    // Other
    objectiveEntriesTop += 25;
  }
}
