/**
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

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;

import de.markusbordihn.easynpc.network.message.MessageActionChange;

public class ActionData {

  // Action Data Tags
  public static final String DATA_ACTION_TAG = "Action";
  public static final String DATA_ACTION_TYPE_TAG = "ActionType";
  public static final String DATA_ACTION_EXECUTE_AS_USER_TAG = "ActionExecuteAsUser";
  public static final String DATA_ACTION_ENABLE_DEBUG_TAG = "ActionEnableDebug";
  public static final String DATA_ACTION_PERMISSION_LEVEL_TAG = "ActionPermissionLevel";

  // Action Data
  private ActionType actionType = ActionType.NONE;
  private String action = "";
  private boolean enableDebug = false;
  private boolean executeAsUser = false;
  private int permissionLevel = 0;

  public ActionData(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public ActionData(ActionType actionType, String action) {
    this(actionType, action, 0);
  }

  public ActionData(ActionType actionType, String action, int permissionLevel) {
    this(actionType, action, permissionLevel, false);
  }

  public ActionData(ActionType actionType, String action, int permissionLevel,
      boolean executeAsUser) {
    this(actionType, action, permissionLevel, executeAsUser, false);
  }

  public ActionData(ActionType actionType, String action, boolean executeAsUser,
      boolean enableDebug) {
    this(actionType, action, 0, executeAsUser, enableDebug);
  }

  public ActionData(ActionType actionType, String action, int permissionLevel,
      boolean executeAsUser, boolean enableDebug) {
    this.action = action;
    this.actionType = actionType;
    this.enableDebug = enableDebug;
    this.executeAsUser = executeAsUser;
    this.permissionLevel = permissionLevel;
  }

  public ActionType getActionType() {
    return this.actionType;
  }

  public String getActionTypeName() {
    return this.actionType.name();
  }

  public String getAction() {
    return this.action;
  }

  public String getAction(LivingEntity entity, ServerPlayer serverPlayer) {
    return ActionUtils.parseAction(this.action, entity, serverPlayer);
  }

  public int getPermissionLevel() {
    return this.permissionLevel;
  }

  public void setPermissionLevel(int permissionLevel) {
    this.permissionLevel = permissionLevel;
  }

  public boolean shouldExecuteAsUser() {
    return this.executeAsUser;
  }

  public void setExecuteAsUser(boolean executeAsUser) {
    this.executeAsUser = executeAsUser;
  }

  public boolean isDebugEnabled() {
    return this.enableDebug;
  }

  public void setEnableDebug(boolean enableDebug) {
    this.enableDebug = enableDebug;
  }

  public boolean hasAction() {
    return this.action != null;
  }

  public boolean hasActionAndNotEmpty() {
    return this.action != null && !this.action.isEmpty();
  }

  public boolean hasActionType() {
    return this.actionType != null && this.actionType != ActionType.NONE;
  }

  public boolean isValid() {
    return this.actionType != ActionType.NONE && this.hasAction();
  }

  public boolean isValidAndNotEmpty() {
    return this.actionType != ActionType.NONE && this.hasActionAndNotEmpty();
  }

  public void load(CompoundTag compoundTag) {
    this.actionType = ActionType.get(compoundTag.getString(DATA_ACTION_TYPE_TAG));
    this.action = compoundTag.getString(DATA_ACTION_TAG);
    this.permissionLevel = compoundTag.getInt(DATA_ACTION_PERMISSION_LEVEL_TAG);
    this.executeAsUser = compoundTag.getBoolean(DATA_ACTION_EXECUTE_AS_USER_TAG);
    this.enableDebug = compoundTag.getBoolean(DATA_ACTION_ENABLE_DEBUG_TAG);
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putString(DATA_ACTION_TYPE_TAG, this.getActionType().name());
    compoundTag.putString(DATA_ACTION_TAG, this.getAction());
    compoundTag.putInt(DATA_ACTION_PERMISSION_LEVEL_TAG, this.getPermissionLevel());
    compoundTag.putBoolean(DATA_ACTION_EXECUTE_AS_USER_TAG, this.shouldExecuteAsUser());
    compoundTag.putBoolean(DATA_ACTION_ENABLE_DEBUG_TAG, this.isDebugEnabled());
    return compoundTag;
  }

  public static void encode(MessageActionChange message, FriendlyByteBuf buffer) {
    ActionData actionData = message.getActionData();
    buffer.writeUtf(actionData.getActionType().name());
    buffer.writeUtf(actionData.getAction());
    buffer.writeInt(actionData.getPermissionLevel());
    buffer.writeBoolean(actionData.shouldExecuteAsUser());
    buffer.writeBoolean(actionData.isDebugEnabled());
  }

  public static ActionData decode(FriendlyByteBuf buffer) {
    String actionType = buffer.readUtf();
    String action = buffer.readUtf();
    int permissionLevel = buffer.readInt();
    boolean executeAsUser = buffer.readBoolean();
    boolean enableDebug = buffer.readBoolean();
    return new ActionData(ActionType.get(actionType), action, permissionLevel, executeAsUser,
        enableDebug);
  }

  @Override
  public boolean equals(Object object) {
    if (object instanceof ActionData) {
      ActionData actionData = (ActionData) object;
      return this.getActionType() == actionData.getActionType()
          && this.getAction().equals(actionData.getAction())
          && this.getPermissionLevel() == actionData.getPermissionLevel()
          && this.shouldExecuteAsUser() == actionData.shouldExecuteAsUser()
          && this.isDebugEnabled() == actionData.isDebugEnabled();
    } else if (this == object) {
      return true;
    }
    return false;
  }

  @Override
  public int hashCode() {
    int result = 17;
    result = 31 * result + this.getActionType().hashCode();
    result = 31 * result + this.getAction().hashCode();
    result = 31 * result + this.getPermissionLevel();
    result = 31 * result + (this.shouldExecuteAsUser() ? 1 : 0);
    result = 31 * result + (this.isDebugEnabled() ? 1 : 0);
    return result;
  }

  public String toString() {
    return "ActionData [actionType=" + this.getActionType() + ", action=" + this.getAction()
        + ", permissionLevel=" + this.getPermissionLevel() + ", executeAsUser="
        + this.shouldExecuteAsUser() + ", enableDebug=" + this.isDebugEnabled() + "]";
  }

}
