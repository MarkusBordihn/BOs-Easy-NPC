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

package de.markusbordihn.easynpc.data.action;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ActionDataEntry {

  public static final String DATA_TAG = "ActionDataEntry";
  public static final String DATA_COMMAND_TAG = "Cmd";
  public static final String DATA_DEBUG_TAG = "Debug";
  public static final String DATA_EXECUTE_AS_USER_TAG = "ExecAsUser";
  public static final String DATA_PERMISSION_LEVEL_TAG = "PermLevel";
  public static final String DATA_BLOCK_POS_TAG = "BlockPos";
  public static final String DATA_TYPE_TAG = "Type";
  public static final int DEFAULT_PERMISSION_LEVEL = 2;
  public static final int MAX_PERMISSION_LEVEL = 2;
  public static final int MIN_PERMISSION_LEVEL = 0;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String DEFAULT_COMMAND = "";
  private ActionDataType actionDataType = ActionDataType.NONE;
  private ConditionDataSet conditionDataSet = new ConditionDataSet();
  private String command = DEFAULT_COMMAND;
  private BlockPos blockPos = BlockPos.ZERO;
  private boolean enableDebug = false;
  private boolean executeAsUser = false;
  private int permissionLevel = DEFAULT_PERMISSION_LEVEL;

  public ActionDataEntry() {
    this(ActionDataType.COMMAND);
  }

  public ActionDataEntry(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public ActionDataEntry(ActionDataType actionDataType) {
    this(actionDataType, DEFAULT_COMMAND, DEFAULT_PERMISSION_LEVEL);
  }

  public ActionDataEntry(ActionDataType actionDataType, String command) {
    this(actionDataType, command, DEFAULT_PERMISSION_LEVEL);
  }

  public ActionDataEntry(ActionDataType actionDataType, String command, int permissionLevel) {
    this(actionDataType, command, permissionLevel, false);
  }

  public ActionDataEntry(
      ActionDataType actionDataType, String command, int permissionLevel, boolean executeAsUser) {
    this(actionDataType, command, permissionLevel, executeAsUser, false);
  }

  public ActionDataEntry(
      ActionDataType actionDataType, String command, boolean executeAsUser, boolean enableDebug) {
    this(actionDataType, command, DEFAULT_PERMISSION_LEVEL, executeAsUser, enableDebug);
  }

  public ActionDataEntry(
      ActionDataType actionDataType,
      String command,
      int permissionLevel,
      boolean executeAsUser,
      boolean enableDebug) {
    this.command = command != null ? command : DEFAULT_COMMAND;
    this.actionDataType = actionDataType;
    this.enableDebug = enableDebug;
    this.executeAsUser = executeAsUser;
    this.permissionLevel = permissionLevel;
  }

  private static int checkPermissionLevel(int permissionLevel) {
    if (permissionLevel > MAX_PERMISSION_LEVEL) {
      log.warn(
          "Permission level {} is too high, will be set to a safe max. level {}",
          permissionLevel,
          MAX_PERMISSION_LEVEL);
      return MAX_PERMISSION_LEVEL;
    } else if (permissionLevel < MIN_PERMISSION_LEVEL) {
      log.warn(
          "Permission level {} is too low, will be set to min. level {}",
          permissionLevel,
          MIN_PERMISSION_LEVEL);
      return MIN_PERMISSION_LEVEL;
    }
    return permissionLevel;
  }

  public ActionDataType getType() {
    return this.actionDataType;
  }

  public void setType(ActionDataType actionDataType) {
    this.actionDataType = actionDataType;
  }

  public BlockPos getBlockPos() {
    return this.blockPos;
  }

  public void setBlockPos(BlockPos blockPos) {
    this.blockPos = blockPos;
  }

  public String getCommand() {
    return this.command;
  }

  public String getAction(LivingEntity entity, ServerPlayer serverPlayer) {
    return ActionUtils.parseAction(this.command, entity, serverPlayer);
  }

  public ConditionDataSet getConditionDataSet() {
    return this.conditionDataSet;
  }

  public void setConditionDataSet(ConditionDataSet conditionDataSet) {
    this.conditionDataSet = conditionDataSet;
  }

  public int getPermissionLevel() {
    return this.permissionLevel;
  }

  public boolean shouldExecuteAsUser() {
    return this.executeAsUser;
  }

  public boolean isDebugEnabled() {
    return this.enableDebug;
  }

  public boolean hasCommand() {
    return this.command != null;
  }

  public boolean hasCommandAndNotEmpty() {
    return this.command != null && !this.command.isEmpty();
  }

  public boolean hasBlockPos() {
    return this.blockPos != null && this.blockPos != BlockPos.ZERO;
  }

  public boolean isValid() {
    return this.actionDataType != ActionDataType.NONE && this.hasCommand();
  }

  public boolean isValidAndNotEmpty() {
    return this.actionDataType != ActionDataType.NONE
        && (this.hasCommandAndNotEmpty()
            || this.hasBlockPos()
            || this.actionDataType == ActionDataType.CLOSE_DIALOG
            || this.actionDataType == ActionDataType.OPEN_TRADING_SCREEN);
  }

  public void load(CompoundTag compoundTag) {
    this.actionDataType = ActionDataType.get(compoundTag.getString(DATA_TYPE_TAG));
    this.blockPos =
        compoundTag.contains(DATA_BLOCK_POS_TAG)
            ? CompoundTagUtils.readBlockPos(compoundTag.getCompound(DATA_BLOCK_POS_TAG))
            : BlockPos.ZERO;
    this.command =
        compoundTag.contains(DATA_COMMAND_TAG)
            ? compoundTag.getString(DATA_COMMAND_TAG)
            : DEFAULT_COMMAND;
    this.permissionLevel =
        compoundTag.contains(DATA_PERMISSION_LEVEL_TAG)
            ? checkPermissionLevel(compoundTag.getInt(DATA_PERMISSION_LEVEL_TAG))
            : DEFAULT_PERMISSION_LEVEL;
    this.executeAsUser =
        compoundTag.contains(DATA_EXECUTE_AS_USER_TAG)
            && compoundTag.getBoolean(DATA_EXECUTE_AS_USER_TAG);
    this.enableDebug =
        compoundTag.contains(DATA_DEBUG_TAG) && compoundTag.getBoolean(DATA_DEBUG_TAG);
    this.conditionDataSet =
        compoundTag.contains(ConditionDataSet.CONDITION_DATA_SET_TAG)
            ? new ConditionDataSet(compoundTag.getCompound(ConditionDataSet.CONDITION_DATA_SET_TAG))
            : new ConditionDataSet();
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putString(DATA_TYPE_TAG, this.getType().name());

    // Only save permission level if it is different from default.
    if (this.getPermissionLevel() != DEFAULT_PERMISSION_LEVEL) {
      compoundTag.putInt(DATA_PERMISSION_LEVEL_TAG, this.getPermissionLevel());
    }

    // Only save block position if it is different from default.
    if (this.getBlockPos() != BlockPos.ZERO) {
      compoundTag.put(DATA_BLOCK_POS_TAG, CompoundTagUtils.writeBlockPos(this.getBlockPos()));
    }

    // Save command, if it is not empty.
    if (this.getCommand() != null && !this.getCommand().trim().isEmpty()) {
      compoundTag.putString(DATA_COMMAND_TAG, this.getCommand().trim());
    }

    // Only save execute as user if it is true.
    if (this.shouldExecuteAsUser()) {
      compoundTag.putBoolean(DATA_EXECUTE_AS_USER_TAG, true);
    }

    // Only save debug if it is true.
    if (this.isDebugEnabled()) {
      compoundTag.putBoolean(DATA_DEBUG_TAG, true);
    }

    // Only save permission level if it is different from default.
    if (this.getPermissionLevel() != DEFAULT_PERMISSION_LEVEL) {
      compoundTag.putInt(DATA_PERMISSION_LEVEL_TAG, this.getPermissionLevel());
    }

    // Store condition data set, if it is not empty.
    if (!this.conditionDataSet.isEmpty()) {
      this.conditionDataSet.save(compoundTag);
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  public UUID getId() {
    String idString = DATA_TAG + hashCode();
    return UUID.nameUUIDFromBytes(idString.getBytes());
  }

  @Override
  public boolean equals(Object object) {
    if (object instanceof ActionDataEntry actionDataEntry) {
      return this.getType() == actionDataEntry.getType()
          && this.getCommand().equals(actionDataEntry.getCommand())
          && this.getBlockPos().equals(actionDataEntry.getBlockPos())
          && this.getPermissionLevel() == actionDataEntry.getPermissionLevel()
          && this.shouldExecuteAsUser() == actionDataEntry.shouldExecuteAsUser()
          && this.isDebugEnabled() == actionDataEntry.isDebugEnabled()
          && this.conditionDataSet.equals(actionDataEntry.getConditionDataSet());
    }
    return false;
  }

  @Override
  public int hashCode() {
    int result = 17;
    result = 31 * result + this.getType().hashCode();
    result = 31 * result + this.getCommand().hashCode();
    result = 31 * result + this.getBlockPos().hashCode();
    result = 31 * result + this.getPermissionLevel();
    result = 31 * result + (this.shouldExecuteAsUser() ? 1 : 0);
    result = 31 * result + (this.isDebugEnabled() ? 1 : 0);
    if (this.conditionDataSet != null && !this.conditionDataSet.isEmpty()) {
      result = 31 * result + this.conditionDataSet.size();
    }
    return result;
  }

  public String toString() {
    return "ActionData [type="
        + this.actionDataType
        + ", cmd="
        + this.command
        + ", blockPos="
        + this.blockPos
        + ", permLvl="
        + this.permissionLevel
        + ", execAsUser="
        + this.executeAsUser
        + ", debug="
        + this.enableDebug
        + (this.conditionDataSet != null && !this.conditionDataSet.isEmpty()
            ? ", conditions=" + this.conditionDataSet
            : "")
        + "]";
  }
}
