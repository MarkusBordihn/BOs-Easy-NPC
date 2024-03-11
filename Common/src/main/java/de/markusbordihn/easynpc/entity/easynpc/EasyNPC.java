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

package de.markusbordihn.easynpc.entity.easynpc;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.custom.CustomDataAccessor;
import de.markusbordihn.easynpc.entity.EasyNPCBaseEntity;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.SpawnerData;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.NeutralMob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.control.LookControl;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface EasyNPC<T extends LivingEntity> extends Npc {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static Class<? extends Entity> getSynchedEntityDataClass() {
    return EasyNPCBaseEntity.class;
  }

  T getEasyNPCEntity();

  int getNPCDataVersion();

  void setNPCDataVersion(int version);

  default LookControl getLookControl() {
    return this instanceof Mob mob ? mob.getLookControl() : null;
  }

  default ActionEventData<T> getEasyNPCActionEventData() {
    return this instanceof ActionEventData<T> actionEventData ? actionEventData : null;
  }

  default AttackData<T> getEasyNPCAttackData() {
    return this instanceof AttackData<T> attackData ? attackData : null;
  }

  default AttributeData<T> getEasyNPCAttributeData() {
    return this instanceof AttributeData<T> attributeData ? attributeData : null;
  }

  default DialogData<T> getEasyNPCDialogData() {
    return this instanceof DialogData<T> dialogData ? dialogData : null;
  }

  default SkinData<T> getEasyNPCSkinData() {
    return this instanceof SkinData<T> skinData ? skinData : null;
  }

  default ModelData<T> getEasyNPCModelData() {
    return this instanceof ModelData<T> modelData ? modelData : null;
  }

  default NavigationData<T> getEasyNPCNavigationData() {
    return this instanceof NavigationData<T> navigationData ? navigationData : null;
  }

  default OwnerData<T> getEasyNPCOwnerData() {
    return this instanceof OwnerData<T> ownerData ? ownerData : null;
  }

  default PresetData<T> getEasyNPCPresetData() {
    return this instanceof PresetData<T> presetData ? presetData : null;
  }

  default SpawnerData<T> getEasyNPCSpawnerData() {
    return this instanceof SpawnerData<T> spawnerData ? spawnerData : null;
  }

  default PathfinderMob getPathfinderMob() {
    return this instanceof PathfinderMob pathfinderMob ? pathfinderMob : null;
  }

  default Level getLevel() {
    return this instanceof Mob mob ? mob.level : null;
  }

  default ServerLevel getServerLevel() {
    return this instanceof Mob mob && mob.level instanceof ServerLevel serverLevel
        ? serverLevel
        : null;
  }

  default boolean isClientSide() {
    return this.getLevel() != null && this.getLevel().isClientSide();
  }

  default LivingEntity getLivingEntity() {
    return this instanceof LivingEntity livingEntity ? livingEntity : null;
  }

  default NeutralMob getNeutralMob() {
    return this instanceof NeutralMob neutralMob ? neutralMob : null;
  }

  default Mob getMob() {
    return this instanceof Mob mob ? mob : null;
  }

  default UUID getUUID() {
    return this instanceof Entity entity ? entity.getUUID() : null;
  }

  GoalSelector getEntityGoalSelector();

  GoalSelector getEntityTargetSelector();

  default CrossbowAttackMob getCrossbowAttackMob() {
    return this instanceof CrossbowAttackMob crossbowAttackMob ? crossbowAttackMob : null;
  }

  default Component getEasyNPCTypeName() {
    return getEasyNPCEntity().getType().getDescription();
  }

  default void handlePlayerJoin(ServerPlayer serverPlayer) {
  }

  default void handlePlayerLeave(ServerPlayer serverPlayer) {
  }

  default void handleLivingEntityJoin(LivingEntity livingEntity) {
  }

  default void handleLivingEntityLeave(LivingEntity livingEntity) {
  }

  default void handleEasyNPCJoin(EasyNPC<?> easyNPCEntity) {
  }

  default void handleEasyNPCLeave(EasyNPC<?> easyNPCEntity) {
  }

  default void defineCustomData() {
  }

  default CompoundTag exportPreset() {
    return getEasyNPCEntity().saveWithoutId(new CompoundTag());
  }

  <V> void setEasyNPCData(EntityDataAccessor<V> entityDataAccessor, V entityData);

  <V> V getEasyNPCData(EntityDataAccessor<V> entityDataAccessor);

  <V> void defineEasyNPCData(EntityDataAccessor<V> entityDataAccessor, V entityData);

  <V> void setEasyNPCCustomData(CustomDataAccessor<V> entityDataAccessor, V entityData);

  <V> V getEasyNPCCustomData(CustomDataAccessor<V> entityDataAccessor);

  <V> void defineEasyNPCCustomData(CustomDataAccessor<V> entityDataAccessor, V entityData);
}
