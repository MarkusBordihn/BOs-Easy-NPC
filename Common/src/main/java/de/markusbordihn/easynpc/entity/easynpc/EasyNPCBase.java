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

import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.GuiData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundData;
import de.markusbordihn.easynpc.entity.easynpc.data.SpawnData;
import de.markusbordihn.easynpc.entity.easynpc.data.SpawnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Saddleable;

public interface EasyNPCBase<E extends PathfinderMob>
    extends Saddleable,
        EasyNPC<E>,
        ActionHandler<E>,
        ActionEventData<E>,
        AttackData<E>,
        AttributeData<E>,
        BaseTickHandler<E>,
        ConfigurationData<E>,
        DialogData<E>,
        GuiData<E>,
        NavigationData<E>,
        ConfigData<E>,
        ObjectiveData<E>,
        OwnerData<E>,
        PresetData<E>,
        ProfessionData<E>,
        RenderData<E>,
        SkinData<E>,
        SoundData<E>,
        SpawnData<E>,
        SpawnerData<E>,
        TickerData<E>,
        TradingData<E>,
        VariantData<E> {

  static void registerEasyNPCDataSerializers() {
    log.info("Register data serializers ...");
    ActionEventData.registerActionEventDataSerializer();
    DialogData.registerDialogDataSerializer();
    ObjectiveData.registerObjectiveDataSerializer();
    ProfessionData.registerProfessionDataSerializer();
    RenderData.registerRenderDataSerializer();
    SoundData.registerSoundDataSerializer();
    SkinData.registerSkinDataSerializer();
    SpawnerData.registerSpawnerDataSerializer();
    TradingData.registerTradingDataSerializer();
  }

  static void registerEasyNPCSyncedData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    AttackData.registerSyncedAttackData(map, entityClass);
    AttributeData.registerSyncedAttributeData(map, entityClass);
    NavigationData.registerSyncedNavigationData(map, entityClass);
    OwnerData.registerSyncedOwnerData(map, entityClass);
    ProfessionData.registerSyncedProfessionData(map, entityClass);
    RenderData.registerSyncedRenderData(map, entityClass);
    SkinData.registerSyncedSkinData(map, entityClass);
    SoundData.registerSyncedSoundData(map, entityClass);
    TradingData.registerSyncedTradingData(map, entityClass);
    VariantData.registerSyncedVariantData(map, entityClass);
  }

  default void registerEasyNPCDefaultHandler(Enum<?> variant) {
    log.info("Register default handler for {} with variant {} ...", this, variant);
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.setVariant(variant);
    }

    // Define server-side custom data.
    if (this.isServerSide()) {
      log.info("Define custom server-side data for {} ...", this);
      ActionEventData<E> actionEventData = getEasyNPCActionEventData();
      if (actionEventData != null) {
        actionEventData.defineCustomActionData();
      }
      DialogData<E> dialogData = getEasyNPCDialogData();
      if (dialogData != null) {
        dialogData.defineCustomDialogData();
      }
      ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
      if (objectiveData != null) {
        objectiveData.defineCustomObjectiveData();
      }
      SpawnerData<E> spawnerData = getEasyNPCSpawnerData();
      if (spawnerData != null) {
        spawnerData.defineCustomSpawnerData();
      }
    }

    // Register default data, if needed.
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.registerDefaultAttributeData(variant);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.registerDefaultSoundData(variant);
    }
  }

  default void defineEasyNPCBaseSyncedData() {
    log.debug("Define synced data for {}", this);

    // First define variant data to ensure that all other data can be linked to the variant.
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.defineSynchedVariantData();
    }

    // Define all other synced data.
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineSynchedActionData();
    }
    AttackData<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.defineSynchedAttackData();
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.defineSynchedAttributeData();
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineSynchedDialogData();
    }
    NavigationData<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.defineSynchedNavigationData();
    }
    OwnerData<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.defineSynchedOwnerData();
    }
    ProfessionData<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.defineSynchedProfessionData();
    }
    RenderData<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.defineSynchedRenderData();
    }
    SkinData<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.defineSynchedSkinData();
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.defineSynchedSoundData();
    }
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.defineSynchedTradingData();
    }
  }

  default void addEasyNPCBaseAdditionalSaveData(CompoundTag compoundTag) {
    log.debug("Add additional save data for {}", this);
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.addAdditionalActionData(compoundTag);
    }
    AttackData<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.addAdditionalAttackData(compoundTag);
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.addAdditionalAttributeData(compoundTag);
    }
    ConfigData<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.addAdditionalConfigData(compoundTag);
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.addAdditionalDialogData(compoundTag);
    }
    NavigationData<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.addAdditionalNavigationData(compoundTag);
    }
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.addAdditionalObjectiveData(compoundTag);
    }
    OwnerData<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.addAdditionalOwnerData(compoundTag);
    }
    ProfessionData<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.addAdditionalProfessionData(compoundTag);
    }
    RenderData<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.addAdditionalRenderData(compoundTag);
    }
    SkinData<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.addAdditionalSkinData(compoundTag);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.addAdditionalSoundData(compoundTag);
    }
    SpawnerData<E> spawnerData = getEasyNPCSpawnerData();
    if (spawnerData != null) {
      spawnerData.addAdditionalSpawnerData(compoundTag);
    }
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.addAdditionalTradingData(compoundTag);
    }
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.addAdditionalVariantData(compoundTag);
    }
  }

  default void readEasyNPCBaseAdditionalSaveData(CompoundTag compoundTag) {
    log.debug("Read additional save data for {} ...", this);

    // First read important data to ensure that all other data can be linked to the variant.
    ConfigData<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.readAdditionalConfigData(compoundTag);
    }
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.readAdditionalVariantData(compoundTag);
    }

    // Read all other synced data.
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.readAdditionalActionData(compoundTag);
    }
    AttackData<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.readAdditionalAttackData(compoundTag);
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.readAdditionalAttributeData(compoundTag);
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.readAdditionalDialogData(compoundTag);
    }
    NavigationData<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.readAdditionalNavigationData(compoundTag);
    }
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.readAdditionalObjectiveData(compoundTag);
    }
    OwnerData<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.readAdditionalOwnerData(compoundTag);
    }
    ProfessionData<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.readAdditionalProfessionData(compoundTag);
    }
    RenderData<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.readAdditionalRenderData(compoundTag);
    }
    SkinData<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.readAdditionalSkinData(compoundTag);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.readAdditionalSoundData(compoundTag);
    }
    SpawnerData<E> spawnerData = getEasyNPCSpawnerData();
    if (spawnerData != null) {
      spawnerData.readAdditionalSpawnerData(compoundTag);
    }
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.readAdditionalTradingData(compoundTag);
    }

    // Register Objectives after all data is loaded.
    if (objectiveData != null) {
      objectiveData.readAdditionalObjectiveData(compoundTag);
    }
  }
}
