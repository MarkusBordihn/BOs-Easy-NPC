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

import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProgressionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ServerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StateDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttributeHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import net.minecraft.core.HolderLookup;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface EasyNPCBase<E extends Mob>
    extends EasyNPC<E>,
        ActionEventDataCapable<E>,
        ActionHandler<E>,
        AttackDataCapable<E>,
        AttributeDataCapable<E>,
        AttributeHandler<E>,
        BaseTickHandler<E>,
        ConfigDataCapable<E>,
        ConfigurationDataCapable<E>,
        DialogDataCapable<E>,
        DisplayAttributeDataCapable<E>,
        FactionDataCapable<E>,
        InventoryDataCapable<E>,
        ModelDataCapable<E>,
        NavigationDataCapable<E>,
        ObjectiveDataCapable<E>,
        OwnerDataCapable<E>,
        PresetDataCapable<E>,
        ProfessionDataCapable<E>,
        ProgressionDataCapable<E>,
        RenderDataCapable<E>,
        ServerDataCapable<E>,
        SkinDataCapable<E>,
        SoundDataCapable<E>,
        StateDataCapable<E>,
        StatusDataCapable<E>,
        TickerDataCapable<E>,
        TradingDataCapable<E>,
        VariantDataCapable<E> {

  @Override
  default <T> void setSynchedEntityData(SynchedDataIndex synchedDataIndex, T data) {
    if (synchedDataIndex.persistent) {
      StatusDataCapable<E> statusData = getEasyNPCStatusData();
      if (statusData != null) {
        statusData.markNPCDataUpdated();
      }
    }
    setSynchedEntityData(synchedDataIndex, data, false);
  }

  default void registerEasyNPCDefaultVariant(Enum<?> variant) {
    log.debug("Register default variant for {} with variant {} ...", this, variant);
    VariantDataCapable<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      if (variantData.getSkinVariantType() == variant) {
        variantData.handleSkinVariantTypeChange(variant);
      } else {
        variantData.setSkinVariantType(variant);
      }
    }
    SoundDataCapable<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.registerDefaultSoundData(variant);
    }
  }

  default SpawnGroupData finalizeEasyNPCSpawn(SpawnGroupData spawnGroupData) {
    log.debug("Finalize spawn for {} ...", this);

    NavigationDataCapable<?> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.applyDefaultNPCHomePosition();
    }

    StatusDataCapable<?> statusData = getEasyNPCStatusData();
    if (statusData == null || !statusData.getStatusDataFlag(StatusDataType.FINALIZED)) {
      registerEasyNPCDefaultData();
    } else {
      log.debug("Skip default data registration for {} ...", this);
    }

    return spawnGroupData;
  }

  default void defineEasyNPCBaseSyncedData(SynchedEntityData.Builder builder) {
    // First define variant data to ensure that all other data can be linked to the variant.
    VariantDataCapable<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.defineSynchedVariantData(builder);
    }

    ActionEventDataCapable<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineSynchedActionData(builder);
    }
    AttackDataCapable<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.defineSynchedAttackData(builder);
    }
    AttributeDataCapable<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.defineSynchedAttributeData(builder);
    }
    DialogDataCapable<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineSynchedDialogData(builder);
    }
    DisplayAttributeDataCapable<E> displayAttributeData = getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.defineSynchedDisplayAttributeData(builder);
    }
    ModelDataCapable<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.defineSynchedModelData(builder);
    }
    NavigationDataCapable<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.defineSynchedNavigationData(builder);
    }
    OwnerDataCapable<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.defineSynchedOwnerData(builder);
    }
    ProfessionDataCapable<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.defineSynchedProfessionData(builder);
    }
    ProgressionDataCapable<E> progressionData = getEasyNPCProgressionData();
    if (progressionData != null) {
      progressionData.defineSynchedProgressionData(builder);
    }
    RenderDataCapable<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.defineSynchedRenderData(builder);
    }
    SkinDataCapable<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.defineSynchedSkinData(builder);
    }
    SoundDataCapable<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.defineSynchedSoundData(builder);
    }
    TradingDataCapable<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.defineSynchedTradingData(builder);
    }
  }

  default void defineEasyNPCBaseServerSideData() {
    if (!this.isServerSideInstance()) {
      return;
    }

    this.getMob().setPersistenceRequired();

    ServerDataCapable<E> serverData = getEasyNPCServerData();
    if (serverData == null) {
      log.error("No server data available for {}", this.getEntityUUID());
      return;
    }
    if (!serverData.hasServerEntityData()) {
      serverData.defineServerEntityData();
    }

    ActionEventDataCapable<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineCustomActionData();
    }
    DialogDataCapable<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineCustomDialogData();
    }
    ObjectiveDataCapable<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.defineCustomObjectiveData();
    }
    StateDataCapable<E> stateData = getEasyNPCStateData();
    if (stateData != null) {
      stateData.defineCustomStateData();
    }
    PresetDataCapable<E> presetData = getEasyNPCPresetData();
    if (presetData != null) {
      presetData.defineCustomPresetData();
    }
    FactionDataCapable<E> factionData = getEasyNPCFactionData();
    if (factionData != null) {
      factionData.defineCustomFactionData();
    }
  }

  default void addEasyNPCBaseAdditionalSaveData(
      ValueOutput valueOutput, HolderLookup.Provider provider) {
    ActionEventDataCapable<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.addAdditionalActionData(valueOutput);
    }
    AttackDataCapable<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.addAdditionalAttackData(valueOutput);
    }
    AttributeDataCapable<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.addAdditionalAttributeData(valueOutput);
    }
    ConfigDataCapable<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.addAdditionalConfigData(valueOutput);
    }
    DialogDataCapable<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.addAdditionalDialogData(valueOutput);
    }
    DisplayAttributeDataCapable<E> displayAttributeData = getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.addAdditionalDisplayAttributeData(valueOutput);
    }
    FactionDataCapable<E> factionData = getEasyNPCFactionData();
    if (factionData != null) {
      factionData.addAdditionalFactionData(valueOutput);
    }
    ModelDataCapable<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.addAdditionalModelData(valueOutput);
    }
    NavigationDataCapable<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.addAdditionalNavigationData(valueOutput);
    }
    ObjectiveDataCapable<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.addAdditionalObjectiveData(valueOutput);
    }
    OwnerDataCapable<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.addAdditionalOwnerData(valueOutput);
    }
    PresetDataCapable<E> presetData = getEasyNPCPresetData();
    if (presetData != null) {
      presetData.addAdditionalPresetData(valueOutput);
    }
    ProfessionDataCapable<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.addAdditionalProfessionData(valueOutput);
    }
    ProgressionDataCapable<E> progressionData = getEasyNPCProgressionData();
    if (progressionData != null) {
      progressionData.addAdditionalProgressionData(valueOutput);
    }
    RenderDataCapable<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.addAdditionalRenderData(valueOutput);
    }
    SkinDataCapable<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.addAdditionalSkinData(valueOutput);
    }
    SoundDataCapable<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.addAdditionalSoundData(valueOutput);
    }
    StateDataCapable<E> stateData = getEasyNPCStateData();
    if (stateData != null) {
      stateData.addAdditionalStateData(valueOutput);
    }
    StatusDataCapable<E> statusData = getEasyNPCStatusData();
    if (statusData != null) {
      statusData.addAdditionalStatusData(valueOutput);
    }
    TradingDataCapable<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.addAdditionalTradingData(valueOutput);
    }
    VariantDataCapable<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.addAdditionalVariantData(valueOutput);
    }
  }

  default void readEasyNPCBaseAdditionalSaveData(
      ValueInput valueInput, HolderLookup.Provider provider) {
    // First read important data to ensure that all other data can be linked to the variant.
    ConfigDataCapable<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.readAdditionalConfigData(valueInput);
    }
    VariantDataCapable<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.readAdditionalVariantData(valueInput);
    }

    ActionEventDataCapable<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.readAdditionalActionData(valueInput);
    }
    AttackDataCapable<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.readAdditionalAttackData(valueInput);
    }
    AttributeDataCapable<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.readAdditionalAttributeData(valueInput);
    }
    DialogDataCapable<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.readAdditionalDialogData(valueInput);
    }
    DisplayAttributeDataCapable<E> displayAttributeData = getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.readAdditionalDisplayAttributeData(valueInput);
    }
    InventoryDataCapable<E> inventoryData = getEasyNPCInventoryData();
    if (inventoryData != null) {
      inventoryData.readAdditionalInventoryData(valueInput);
    }
    FactionDataCapable<E> factionData = getEasyNPCFactionData();
    if (factionData != null) {
      factionData.readAdditionalFactionData(valueInput);
    }
    ModelDataCapable<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.readAdditionalModelData(valueInput);
    }
    NavigationDataCapable<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.readAdditionalNavigationData(valueInput);
    }
    OwnerDataCapable<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.readAdditionalOwnerData(valueInput);
    }
    PresetDataCapable<E> presetData = getEasyNPCPresetData();
    if (presetData != null) {
      presetData.readAdditionalPresetData(valueInput);
    }
    ProfessionDataCapable<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.readAdditionalProfessionData(valueInput);
    }
    ProgressionDataCapable<E> progressionData = getEasyNPCProgressionData();
    if (progressionData != null) {
      progressionData.readAdditionalProgressionData(valueInput);
    }
    RenderDataCapable<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.readAdditionalRenderData(valueInput);
    }
    SkinDataCapable<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.readAdditionalSkinData(valueInput);
    }
    SoundDataCapable<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.readAdditionalSoundData(valueInput);
    }
    StateDataCapable<E> stateData = getEasyNPCStateData();
    if (stateData != null) {
      stateData.readAdditionalStateData(valueInput);
    }
    StatusDataCapable<E> statusData = getEasyNPCStatusData();
    if (statusData != null) {
      statusData.readAdditionalStatusData(valueInput);
    }
    TradingDataCapable<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.readAdditionalTradingData(valueInput);
    }

    ObjectiveDataCapable<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.readAdditionalObjectiveData(valueInput);
    }

    if (navigationData != null) {
      navigationData.refreshNavigation();
    }
  }
}
