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
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ActionData {

  // Limits
  public static final int MAX_PERMISSION_LEVEL = 2;
  public static final int DEFAULT_PERMISSION_LEVEL = 2;
  public static final int MIN_PERMISSION_LEVEL = 0;
  // Action Data Tags
  public static final String DATA_COMMAND_TAG = "Cmd";
  public static final String DATA_DEBUG_TAG = "Debug";
  public static final String DATA_EXECUTE_AS_USER_TAG = "ExecAsUser";
  public static final String DATA_PERMISSION_LEVEL_TAG = "PermLevel";
  public static final String DATA_TYPE_TAG = "Type";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String DEFAULT_COMMAND = "";
  // Action Data
  private ActionType actionType = ActionType.NONE;
  private String command = "";
  private boolean enableDebug = false;
  private boolean executeAsUser = false;
  private int permissionLevel = DEFAULT_PERMISSION_LEVEL;

  public ActionData(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public ActionData(ActionType actionType) {
    this(actionType, DEFAULT_COMMAND, DEFAULT_PERMISSION_LEVEL);
  }

  public ActionData(ActionType actionType, String command) {
    this(actionType, command, DEFAULT_PERMISSION_LEVEL);
  }

  public ActionData(ActionType actionType, String command, int permissionLevel) {
    this(actionType, command, permissionLevel, false);
  }

  public ActionData(
      ActionType actionType, String command, int permissionLevel, boolean executeAsUser) {
    this(actionType, command, permissionLevel, executeAsUser, false);
  }

  public ActionData(
      ActionType actionType, String command, boolean executeAsUser, boolean enableDebug) {
    this(actionType, command, DEFAULT_PERMISSION_LEVEL, executeAsUser, enableDebug);
  }

  public ActionData(
      ActionType actionType,
      String command,
      int permissionLevel,
      boolean executeAsUser,
      boolean enableDebug) {
    this.command = command != null ? command : DEFAULT_COMMAND;
    this.actionType = actionType;
    this.enableDebug = enableDebug;
    this.executeAsUser = executeAsUser;
    this.permissionLevel = permissionLevel;
  }

  public ActionType getType() {
    return this.actionType;
  }

  public void setType(ActionType actionType) {
    this.actionType = actionType;
  }

  public String getCommand() {
    return this.command;
  }

  public String getAction(LivingEntity entity, ServerPlayer serverPlayer) {
    return ActionUtils.parseAction(this.command, entity, serverPlayer);
  }

  public int getPermissionLevel() {
    return this.permissionLevel;
  }

  public void setPermissionLevel(int permissionLevel) {
    if (permissionLevel > MAX_PERMISSION_LEVEL) {
      log.warn(
          "Permission level {} is too high, will be set to a safe max. level {}",
          permissionLevel,
          MAX_PERMISSION_LEVEL);
      this.permissionLevel = MAX_PERMISSION_LEVEL;
    } else if (permissionLevel < MIN_PERMISSION_LEVEL) {
      log.warn(
          "Permission level {} is too low, will be set to min. level {}",
          permissionLevel,
          MIN_PERMISSION_LEVEL);
      this.permissionLevel = MIN_PERMISSION_LEVEL;
    } else {
      this.permissionLevel = permissionLevel;
    }
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

  public boolean isValid() {
    return this.actionType != ActionType.NONE && this.hasCommand();
  }

  public boolean isValidAndNotEmpty() {
    return this.actionType != ActionType.NONE
        && (this.hasCommandAndNotEmpty() || this.actionType == ActionType.OPEN_TRADING_SCREEN);
  }

  public void load(CompoundTag compoundTag) {
    this.actionType = ActionType.get(compoundTag.getString(DATA_TYPE_TAG));
    this.command = compoundTag.getString(DATA_COMMAND_TAG);
    this.permissionLevel =
        compoundTag.contains(DATA_PERMISSION_LEVEL_TAG)
            ? compoundTag.getInt(DATA_PERMISSION_LEVEL_TAG)
            : DEFAULT_PERMISSION_LEVEL;
    this.executeAsUser =
        compoundTag.contains(DATA_EXECUTE_AS_USER_TAG)
            && compoundTag.getBoolean(DATA_EXECUTE_AS_USER_TAG);
    this.enableDebug =
        compoundTag.contains(DATA_DEBUG_TAG) && compoundTag.getBoolean(DATA_DEBUG_TAG);
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putString(DATA_TYPE_TAG, this.getType().name());
    compoundTag.putString(DATA_COMMAND_TAG, this.getCommand());
    compoundTag.putInt(DATA_PERMISSION_LEVEL_TAG, this.getPermissionLevel());

    // Only save execute as user if it is true.
    if (this.shouldExecuteAsUser()) {
      compoundTag.putBoolean(DATA_EXECUTE_AS_USER_TAG, this.shouldExecuteAsUser());
    }

    // Only save debug if it is true.
    if (this.isDebugEnabled()) {
      compoundTag.putBoolean(DATA_DEBUG_TAG, this.isDebugEnabled());
    }

    // Only save permission level if it is different from default.
    if (this.getPermissionLevel() != DEFAULT_PERMISSION_LEVEL) {
      compoundTag.putInt(DATA_PERMISSION_LEVEL_TAG, this.getPermissionLevel());
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public boolean equals(Object object) {
    if (object instanceof ActionData actionData) {
      return this.getType() == actionData.getType()
          && this.getCommand().equals(actionData.getCommand())
          && this.getPermissionLevel() == actionData.getPermissionLevel()
          && this.shouldExecuteAsUser() == actionData.shouldExecuteAsUser()
          && this.isDebugEnabled() == actionData.isDebugEnabled();
    }
    return false;
  }

  @Override
  public int hashCode() {
    int result = 17;
    result = 31 * result + this.getType().hashCode();
    result = 31 * result + this.getCommand().hashCode();
    result = 31 * result + this.getPermissionLevel();
    result = 31 * result + (this.shouldExecuteAsUser() ? 1 : 0);
    result = 31 * result + (this.isDebugEnabled() ? 1 : 0);
    return result;
  }

  public String toString() {
    return "ActionData [type="
        + this.actionType
        + ", cmd="
        + this.command
        + ", permLvl="
        + this.permissionLevel
        + ", execAsUser="
        + this.executeAsUser
        + ", debug="
        + this.enableDebug
        + "]";
  }
}
